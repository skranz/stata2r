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
      r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, list(is_by_group = FALSE))
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
      paste0("res_L", line_num, " = scmd_estimation_effects(", paste(args, collapse = ", "), ")")
    )

    if ("e(sample)" %in% needed_e) {
      code_lines = c(
        code_lines,
        paste0(
          "assign(",
          quote_for_r_literal(paste0("stata_e_sample_L", line_num)),
          ", res_L",
          line_num,
          "$e_sample, envir = stata2r_env)"
        )
      )
    }
    if ("e(N)" %in% needed_e) {
      code_lines = c(
        code_lines,
        paste0(
          "assign(",
          quote_for_r_literal(paste0("stata_e_L", line_num, "_N")),
          ", res_L",
          line_num,
          "$e_N, envir = stata2r_env)"
        )
      )
    }
    if ("e(r2)" %in% needed_e) {
      code_lines = c(
        code_lines,
        paste0(
          "assign(",
          quote_for_r_literal(paste0("stata_e_L", line_num, "_r2")),
          ", res_L",
          line_num,
          "$e_r2, envir = stata2r_env)"
        )
      )
    }
    if ("e(df_r)" %in% needed_e) {
      code_lines = c(
        code_lines,
        paste0(
          "assign(",
          quote_for_r_literal(paste0("stata_e_L", line_num, "_df_r")),
          ", res_L",
          line_num,
          "$e_df_r, envir = stata2r_env)"
        )
      )
    }
    if ("e(rmse)" %in% needed_e) {
      code_lines = c(
        code_lines,
        paste0(
          "assign(",
          quote_for_r_literal(paste0("stata_e_L", line_num, "_rmse")),
          ", res_L",
          line_num,
          "$e_rmse, envir = stata2r_env)"
        )
      )
    }
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
  if (length(dep_actual) > 0) dep_actual = dep_actual[1] else dep_actual = character(0)

  model_vars_actual = character(0)
  if (length(model_vars) > 0) {
    model_vars_actual = unlist(lapply(model_vars, function(v) {
      expanded = expand_varlist(v, names(data))
      if (length(expanded) > 0) expanded else character(0)
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
      # Use only the first matched column for the dependent variable in formula
      formula_str = paste0("`", dep_actual, "` ~ ", paste(paste0("`", formula_terms, "`"), collapse = " + "))
      mod = stats::lm(stats::as.formula(formula_str), data = data[e_sample == 1, , drop = FALSE])
      sm = summary(mod)

      if ("e(r2)" %in% needed_e) res$e_r2 = sm$r.squared
      if ("e(df_r)" %in% needed_e) res$e_df_r = mod$df.residual
      if ("e(rmse)" %in% needed_e) res$e_rmse = sm$sigma
    } else {
      if ("e(r2)" %in% needed_e) res$e_r2 = NA_real_
      if ("e(df_r)" %in% needed_e) res$e_df_r = NA_real_
      if ("e(rmse)" %in% needed_e) res$e_rmse = NA_real_
    }
  }

  return(res)
}
