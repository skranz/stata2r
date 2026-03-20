# FILE: R/t_regress.R

# 1. Parsing Phase
s2r_p_regress = function(rest_of_cmd) {
  restore.point("s2r_p_regress")
  no_opts = stringi::stri_replace_all_regex(rest_of_cmd, ",\\s*\\w+\\(?[^)]*\\)?", "")
  no_opts = stringi::stri_replace_all_regex(no_opts, ",\\s*robust\\b", "")

  parsed = s2r_parse_if_in(no_opts)
  vars = stringi::stri_split_regex(stringi::stri_trim_both(parsed$base_str), "\\s+")[[1]]
  vars = vars[vars != ""]

  list(
    dep_var = if (length(vars) > 0) vars[1] else NA_character_,
    indep_vars = if (length(vars) > 1) vars[-1] else character(0),
    if_str = parsed$if_str
  )
}

# 2. Code Generation Phase
t_regress = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_regress")
  needed_e = unlist(cmd_obj$e_results_needed)
  if (length(needed_e) == 0) {
    return(paste0("# regress at line ", line_num, " is no-op (no e() used)."))
  }

  parsed = s2r_p_regress(rest_of_cmd)
  if (is.na(parsed$dep_var)) {
    return(paste0("# regress missing depvar at line ", line_num))
  }

  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) {
    r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, list(is_by_group = FALSE))
  }

  needed_r_str = paste0("c('", paste(needed_e, collapse = "','"), "')")
  indep_str = if (length(parsed$indep_vars) > 0) {
    paste0("c('", paste(parsed$indep_vars, collapse = "','"), "')")
  } else {
    "character(0)"
  }

  args = c(
    "data = data",
    paste0("dep_var = ", quote_for_r_literal(parsed$dep_var)),
    paste0("indep_vars = ", indep_str),
    paste0("needed_e = ", needed_r_str)
  )
  if (!is.na(r_if_cond)) {
    args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  }

  r_code = paste0("res_L", line_num, " = scmd_regress(", paste(args, collapse = ", "), ")")

  if ("e(sample)" %in% needed_e) {
    r_code = paste0(
      r_code,
      "\nassign(",
      quote_for_r_literal(paste0("stata_e_sample_L", line_num)),
      ", res_L",
      line_num,
      "$e_sample, envir = stata2r_env)"
    )
  }
  if ("e(N)" %in% needed_e) {
    r_code = paste0(
      r_code,
      "\nassign(",
      quote_for_r_literal(paste0("stata_e_L", line_num, "_N")),
      ", res_L",
      line_num,
      "$e_N, envir = stata2r_env)"
    )
  }
  if ("e(r2)" %in% needed_e) {
    r_code = paste0(
      r_code,
      "\nassign(",
      quote_for_r_literal(paste0("stata_e_L", line_num, "_r2")),
      ", res_L",
      line_num,
      "$e_r2, envir = stata2r_env)"
    )
  }
  if ("e(df_r)" %in% needed_e) {
    r_code = paste0(
      r_code,
      "\nassign(",
      quote_for_r_literal(paste0("stata_e_L", line_num, "_df_r")),
      ", res_L",
      line_num,
      "$e_df_r, envir = stata2r_env)"
    )
  }
  if ("e(rmse)" %in% needed_e) {
    r_code = paste0(
      r_code,
      "\nassign(",
      quote_for_r_literal(paste0("stata_e_L", line_num, "_rmse")),
      ", res_L",
      line_num,
      "$e_rmse, envir = stata2r_env)"
    )
  }

  return(r_code)
}

# 3. Runtime Execution Phase
scmd_regress = function(data, dep_var, indep_vars, needed_e, r_if_cond = NA_character_) {
  restore.point("scmd_regress")
  mask = rep(TRUE, nrow(data))
  if (!is.na(r_if_cond) && r_if_cond != "") {
    mask = mask & s2r_eval_cond(data, r_if_cond)
  }

  all_vars = expand_varlist(paste(c(dep_var, indep_vars), collapse = " "), names(data))
  cc_mask = stats::complete.cases(data[, all_vars, drop = FALSE])

  e_sample = as.integer(mask & cc_mask)
  res = list(e_sample = e_sample)

  if ("e(N)" %in% needed_e) {
    res$e_N = sum(e_sample)
  }

  other = setdiff(needed_e, c("e(sample)", "e(N)"))
  if (length(other) > 0) {
    formula_str = paste0(
      "`",
      all_vars[1],
      "` ~ ",
      if (length(all_vars) > 1) paste(paste0("`", all_vars[-1], "`"), collapse = " + ") else "1"
    )
    mod = stats::lm(as.formula(formula_str), data = data[e_sample == 1, , drop = FALSE])
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
  }

  return(res)
}
