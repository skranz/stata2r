Yes, I think you can remove those line suffixes cleanly, and it is the right design.

The core reason is semantic: in Stata there is only one current `e()` namespace and one current `r()` namespace. A later estimation command overwrites the current `e()` results, and a later r-class command overwrites the current `r()` results. So names like `stata_e_sample_L3` are not modeling Stata itself; they are just an implementation detail. That detail becomes brittle once translated code is reassembled and original line numbers stop being stable.

What should stay is the backward dependency analysis in `s2r_check_mod_df()` and the lookup logic in `translate_stata_expression_with_r_values()`. That logic is still useful, because it answers two separate questions:

1. Which earlier commands actually need to be translated for side effects?
2. At a later line, which immediately preceding relevant producer is the active Stata `e()` or `r()` source?

What should change is only the runtime storage. Instead of storing results under line-specific names, store them in stable slots like:

* `stata2r_env$stata_e_sample`
* `stata2r_env$stata_e_N`
* `stata2r_env$stata_r_mean`

and overwrite those slots whenever a relevant command runs.

I would also replace the temporary generated names `res_L...` by generic temporary names like `s2r_est_res` and `s2r_r_res`, because those are only consumed immediately and do not need line identities either.

I did not touch the `tempfile` line suffixes. Those are a different case: there the suffix is encoding a generated local name and helps avoid collisions. The brittle part here is specifically the emulated global-current `e()` / `r()` state.

Here are the concrete file rewrites.

!MODIFICATION s2r_store_results.R
scope = "file"
file = "R/s2r_store_results.R"
is_new_file = true
description = "Add stable runtime storage helpers for current e() and r() results without line-number suffixes."
----------------------------------------------------------------------------------------------------------------

```r
# Stable storage helpers for Stata-style current e() and r() results

s2r_store_e_results = function(res) {
  restore.point("s2r_store_e_results")

  assign("stata_e_sample", NULL, envir = stata2r_env)
  assign("stata_e_N", NA_real_, envir = stata2r_env)
  assign("stata_e_r2", NA_real_, envir = stata2r_env)
  assign("stata_e_df_r", NA_real_, envir = stata2r_env)
  assign("stata_e_rmse", NA_real_, envir = stata2r_env)

  if (!is.null(res$e_sample)) {
    assign("stata_e_sample", res$e_sample, envir = stata2r_env)
  }
  if (!is.null(res$e_N)) {
    assign("stata_e_N", res$e_N, envir = stata2r_env)
  }
  if (!is.null(res$e_r2)) {
    assign("stata_e_r2", res$e_r2, envir = stata2r_env)
  }
  if (!is.null(res$e_df_r)) {
    assign("stata_e_df_r", res$e_df_r, envir = stata2r_env)
  }
  if (!is.null(res$e_rmse)) {
    assign("stata_e_rmse", res$e_rmse, envir = stata2r_env)
  }

  invisible(res)
}

s2r_store_r_results = function(res) {
  restore.point("s2r_store_r_results")

  assign("stata_r_N", NA_real_, envir = stata2r_env)
  assign("stata_r_mean", NA_real_, envir = stata2r_env)
  assign("stata_r_sd", NA_real_, envir = stata2r_env)
  assign("stata_r_min", NA_real_, envir = stata2r_env)
  assign("stata_r_max", NA_real_, envir = stata2r_env)
  assign("stata_r_sum", NA_real_, envir = stata2r_env)
  assign("stata_r_p50", NA_real_, envir = stata2r_env)

  if (!is.null(res$r_N)) {
    assign("stata_r_N", res$r_N, envir = stata2r_env)
  }
  if (!is.null(res$r_mean)) {
    assign("stata_r_mean", res$r_mean, envir = stata2r_env)
  }
  if (!is.null(res$r_sd)) {
    assign("stata_r_sd", res$r_sd, envir = stata2r_env)
  }
  if (!is.null(res$r_min)) {
    assign("stata_r_min", res$r_min, envir = stata2r_env)
  }
  if (!is.null(res$r_max)) {
    assign("stata_r_max", res$r_max, envir = stata2r_env)
  }
  if (!is.null(res$r_sum)) {
    assign("stata_r_sum", res$r_sum, envir = stata2r_env)
  }
  if (!is.null(res$r_p50)) {
    assign("stata_r_p50", res$r_p50, envir = stata2r_env)
  }

  invisible(res)
}
```

!END_MODIFICATION s2r_store_results.R

!MODIFICATION t_estimation_cmd.R
scope = "file"
file = "R/t_estimation_cmd.R"
is_new_file = false
description = "Rewrite estimation side-effect translation to store current e() results in stable runtime slots instead of line-numbered variables."
---------------------------------------------------------------------------------------------------------------------------------------------------

```r
# Shared estimation-side-effects translation and runtime helpers

t_estimation_cmd = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context, estimator = cmd_obj$stata_cmd) {
  restore.point("t_estimation_cmd")

  needed_e = unlist(cmd_obj$e_results_needed)
  need_xi = isTRUE(cmd_obj$need_xi)

  if (length(needed_e) == 0 && !need_xi) {
    return(paste0("# ", estimator, " at line ", line_num, " is no-op (no later-used side effects)."))
  }

  parsed = s2r_p_estimation_cmd(rest_of_cmd, estimator = estimator)
  if (is.na(parsed$dep_var) && length(parsed$xi_specs) == 0) {
    return(paste0("# Failed to parse estimation command at line ", line_num, ": ", cmd_obj$do_code))
  }

  code_lines = character(0)

  if (need_xi) {
    if (length(parsed$xi_specs) == 0) {
      return(paste0("# Failed to parse xi side effects for estimation command at line ", line_num, ": ", cmd_obj$do_code))
    }

    for (spec in parsed$xi_specs) {
      if (is.null(spec$var2) || is.na(spec$var2) || spec$var2 == "") {
        code_lines = c(
          code_lines,
          paste0(
            "data = scmd_xi(data = data, var1 = ",
            quote_for_r_literal(spec$var1),
            ")"
          )
        )
      } else {
        code_lines = c(
          code_lines,
          paste0(
            "data = scmd_xi(data = data, var1 = ",
            quote_for_r_literal(spec$var1),
            ", var2 = ",
            quote_for_r_literal(spec$var2),
            ")"
          )
        )
      }
    }
  }

  if (length(needed_e) > 0) {
    r_if_cond = NA_character_
    if (!is.na(parsed$if_str) && parsed$if_str != "") {
      r_if_cond = translate_stata_expression_with_r_values(
        parsed$if_str,
        line_num,
        cmd_df,
        list(is_by_group = FALSE)
      )
    }

    args = c(
      "data = data",
      paste0("dep_var = ", quote_for_r_literal(parsed$dep_var)),
      paste0("model_vars = ", s2r_chr_vec_to_r(parsed$model_vars)),
      paste0("needed_e = ", s2r_chr_vec_to_r(needed_e)),
      paste0("estimator = ", quote_for_r_literal(estimator)),
      paste0("formula_terms = ", s2r_chr_vec_to_r(parsed$indep_terms))
    )
    if (!is.na(r_if_cond)) {
      args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
    }

    code_lines = c(
      code_lines,
      paste0("s2r_est_res = scmd_estimation_effects(", paste(args, collapse = ", "), ")"),
      "s2r_store_e_results(s2r_est_res)"
    )
  }

  paste(code_lines, collapse = "\n")
}

scmd_estimation_effects = function(data, dep_var, model_vars, needed_e, r_if_cond = NA_character_, estimator = "regress", formula_terms = character(0)) {
  restore.point("scmd_estimation_effects")

  mask = rep(TRUE, nrow(data))
  if (!is.na(r_if_cond) && r_if_cond != "") {
    mask = mask & s2r_eval_cond(data, r_if_cond, envir = parent.frame())
  }

  dep_actual = expand_varlist(dep_var, names(data))
  if (length(dep_actual) > 0) {
    dep_actual = dep_actual[1]
  } else {
    dep_actual = character(0)
  }

  model_vars_actual = character(0)
  if (length(model_vars) > 0) {
    model_vars_actual = unlist(lapply(model_vars, function(v) {
      expanded = expand_varlist(v, names(data))
      if (length(expanded) > 0) {
        expanded
      } else {
        character(0)
      }
    }))
  }
  model_vars_actual = unique(model_vars_actual)

  all_vars = unique(c(dep_actual, model_vars_actual))
  if (length(all_vars) > 0) {
    cc_mask = stats::complete.cases(data[, all_vars, drop = FALSE])
  } else {
    cc_mask = rep(TRUE, nrow(data))
  }

  e_sample = as.integer(mask & cc_mask)
  res = list(e_sample = e_sample)

  if ("e(N)" %in% needed_e) {
    res$e_N = sum(e_sample)
  }

  other_e = setdiff(needed_e, c("e(sample)", "e(N)"))
  if (length(other_e) > 0) {
    can_fit_lm = estimator %in% c("regress", "areg", "xtreg", "reghdfe", "ivregress") &&
      length(formula_terms) > 0 &&
      all(stringi::stri_detect_regex(formula_terms, "^[A-Za-z_][A-Za-z0-9_]*$"))

    if (can_fit_lm && sum(e_sample) > 0) {
      formula_str = paste0(
        "`", dep_actual, "` ~ ",
        paste(paste0("`", formula_terms, "`"), collapse = " + ")
      )
      mod = stats::lm(
        stats::as.formula(formula_str),
        data = data[e_sample == 1, , drop = FALSE]
      )
      sm = summary(mod)

      if ("e(r2)" %in% needed_e) {
        res$e_r2 = sm$r.squared
      }
      if ("e(df_r)" %in% needed_e) {
        res$e_df_r = mod$df.residual
      }
      if ("e(rmse)" %in% needed_e) {
        res$e_rmse = sm$sigma
      }
    } else {
      if ("e(r2)" %in% needed_e) {
        res$e_r2 = NA_real_
      }
      if ("e(df_r)" %in% needed_e) {
        res$e_df_r = NA_real_
      }
      if ("e(rmse)" %in% needed_e) {
        res$e_rmse = NA_real_
      }
    }
  }

  return(res)
}
```

!END_MODIFICATION t_estimation_cmd.R

!MODIFICATION t_summarize.R
scope = "file"
file = "R/t_summarize.R"
is_new_file = false
description = "Rewrite summarize side-effect translation to store current r() results in stable runtime slots instead of line-numbered variables."
--------------------------------------------------------------------------------------------------------------------------------------------------

```r
# FILE: R/t_summarize.R

# 1. Parsing Phase
s2r_p_summarize = function(rest_of_cmd) {
  restore.point("s2r_p_summarize")
  parts = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*([^,]*?)(?:,\\s*(.*))?$")
  parsed = s2r_parse_if_in(stringi::stri_trim_both(parts[1, 2]))

  vars = stringi::stri_split_regex(stringi::stri_trim_both(parsed$base_str), "\\s+")[[1]]
  list(varlist = vars[vars != ""], if_str = parsed$if_str, options = parts[1, 3])
}

# 2. Code Generation Phase
t_summarize = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_summarize")

  needed_r = unlist(cmd_obj$r_results_needed)
  if (length(needed_r) == 0) {
    return(paste0("# summarize at line ", line_num, " is no-op (no r() used)."))
  }

  parsed = s2r_p_summarize(rest_of_cmd)

  var_for_r = NA_character_
  if (length(parsed$varlist) > 0) {
    var_for_r = parsed$varlist[length(parsed$varlist)]
  }

  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) {
    r_if_cond = translate_stata_expression_with_r_values(
      parsed$if_str,
      line_num,
      cmd_df,
      list(is_by_group = FALSE)
    )
  }

  args = c("data = data", paste0("needed_r = ", s2r_chr_vec_to_r(needed_r)))
  if (!is.na(var_for_r)) {
    args = c(args, paste0("var_for_r = ", quote_for_r_literal(var_for_r)))
  }
  if (!is.na(r_if_cond)) {
    args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  }

  r_code = paste0("s2r_r_res = scmd_summarize(", paste(args, collapse = ", "), ")")
  r_code = paste0(r_code, "\ns2r_store_r_results(s2r_r_res)")

  return(r_code)
}

# 3. Runtime Execution Phase
scmd_summarize = function(data, needed_r, var_for_r = NA_character_, r_if_cond = NA_character_) {
  restore.point("scmd_summarize")

  if (!is.na(r_if_cond) && r_if_cond != "") {
    data = data[s2r_eval_cond(data, r_if_cond, envir = parent.frame()), , drop = FALSE]
  }

  res = list()
  if (is.na(var_for_r)) {
    if ("r(N)" %in% needed_r) {
      res$r_N = nrow(data)
    }
  } else {
    v = expand_varlist(var_for_r, names(data))[1]
    col = data[[v]]

    if ("r(N)" %in% needed_r) {
      res$r_N = sum(!is.na(col))
    }
    if ("r(mean)" %in% needed_r) {
      res$r_mean = mean(col, na.rm = TRUE)
    }
    if ("r(sd)" %in% needed_r) {
      res$r_sd = stats::sd(col, na.rm = TRUE)
    }
    if ("r(min)" %in% needed_r) {
      res$r_min = min(col, na.rm = TRUE)
    }
    if ("r(max)" %in% needed_r) {
      res$r_max = max(col, na.rm = TRUE)
    }
    if ("r(sum)" %in% needed_r) {
      res$r_sum = sum(col, na.rm = TRUE)
    }
    if ("r(p50)" %in% needed_r) {
      res$r_p50 = stats::median(col, na.rm = TRUE)
    }
  }

  return(res)
}
```

!END_MODIFICATION t_summarize.R

!MODIFICATION translate_stata_expression_with_r_values.R
scope = "file"
file = "R/translate_stata_expression_with_r_values.R"
is_new_file = false
description = "Rewrite macro-to-R mapping so later expressions refer to stable current e() and r() slots instead of line-numbered temporary variables."
-------------------------------------------------------------------------------------------------------------------------------------------------------

```r
translate_stata_expression_with_r_values = function(stata_expr, line_num, cmd_df, context) {
  restore.point("translate_stata_expression_with_r_values")

  r_value_mappings = list()

  if (line_num > 1L) {
    prev_cmds = cmd_df$stata_cmd[1:(line_num - 1L)]
    prev_trans = cmd_df$do_translate[1:(line_num - 1L)]

    # Handle r() values from the most recent translated r-class producer
    r_producers = c("summarize", "su", "tabulate", "tab")
    r_matches = which(prev_cmds %in% r_producers & prev_trans)

    if (length(r_matches) > 0) {
      prev_cmd_obj = cmd_df[r_matches[length(r_matches)], , drop = FALSE]
      needed_r = unlist(prev_cmd_obj$r_results_needed)

      if ("r(N)" %in% needed_r) {
        r_value_mappings[["r(N)"]] = "stata2r_env$stata_r_N"
      }
      if ("r(mean)" %in% needed_r) {
        r_value_mappings[["r(mean)"]] = "stata2r_env$stata_r_mean"
      }
      if ("r(sd)" %in% needed_r) {
        r_value_mappings[["r(sd)"]] = "stata2r_env$stata_r_sd"
      }
      if ("r(min)" %in% needed_r) {
        r_value_mappings[["r(min)"]] = "stata2r_env$stata_r_min"
      }
      if ("r(max)" %in% needed_r) {
        r_value_mappings[["r(max)"]] = "stata2r_env$stata_r_max"
      }
      if ("r(sum)" %in% needed_r) {
        r_value_mappings[["r(sum)"]] = "stata2r_env$stata_r_sum"
      }
      if ("r(p50)" %in% needed_r) {
        r_value_mappings[["r(p50)"]] = "stata2r_env$stata_r_p50"
      }
    }

    # Handle e() values from the most recent translated estimation producer
    e_matches = which(prev_cmds %in% stata_estimation_cmds & prev_trans)

    if (length(e_matches) > 0) {
      prev_cmd_obj_e = cmd_df[e_matches[length(e_matches)], , drop = FALSE]
      needed_e = unlist(prev_cmd_obj_e$e_results_needed)

      if ("e(sample)" %in% needed_e) {
        r_value_mappings[["e(sample)"]] = "stata2r_env$stata_e_sample"
      }
      if ("e(N)" %in% needed_e) {
        r_value_mappings[["e(N)"]] = "stata2r_env$stata_e_N"
      }
      if ("e(r2)" %in% needed_e) {
        r_value_mappings[["e(r2)"]] = "stata2r_env$stata_e_r2"
      }
      if ("e(df_r)" %in% needed_e) {
        r_value_mappings[["e(df_r)"]] = "stata2r_env$stata_e_df_r"
      }
      if ("e(rmse)" %in% needed_e) {
        r_value_mappings[["e(rmse)"]] = "stata2r_env$stata_e_rmse"
      }
    }
  }

  translated_expr = translate_stata_expression_to_r(
    stata_expr,
    context = context,
    r_value_mappings = r_value_mappings
  )

  return(translated_expr)
}
```

!END_MODIFICATION translate_stata_expression_with_r_values.R

This gives you stable current-result storage, keeps the existing dependency analysis, and removes the line-number coupling from `e()` / `r()` emulation.
