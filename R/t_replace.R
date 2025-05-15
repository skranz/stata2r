# Translate Stata 'replace' command
# Stata: replace oldvar = expression [if condition]
t_replace = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  # Parsing is similar to generate, but `new_var` is an existing variable.
  match = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*([^=\\s]+)\\s*=\\s*(.*?)(?:\\s+if\\s+(.*))?$")

  if (is.na(match[1,1])) {
    return(paste0("# Failed to parse replace command: ", rest_of_cmd))
  }

  var_to_replace = stringi::stri_trim_both(match[1,2])
  stata_expr = stringi::stri_trim_both(match[1,3])
  stata_if_cond = stringi::stri_trim_both(match[1,4]) # Might be NA

  r_expr = translate_stata_expression_with_r_values(stata_expr, line_num, cmd_df, context)

  r_if_cond = NA_character_
  if (!is.na(stata_if_cond) && stata_if_cond != "") {
    # For replace, the `if` condition is on the whole dataset typically (unless `by` prefix)
    # However, the context for _n, _N inside the expression might depend on `by` prefix.
    # The `if` condition itself should be evaluated without `by` group context for row selection.
    r_if_cond = translate_stata_expression_with_r_values(stata_if_cond, line_num, cmd_df, context = list(is_by_group = FALSE))
  }

  # Construct R code
  by_vars_r = NULL
  if (cmd_obj$is_by_prefix && !is.na(cmd_obj$by_vars)) {
    by_vars_r = paste0("c(", paste0('"', stringi::stri_split_fixed(cmd_obj$by_vars, " ")[[1]], '"', collapse=", "), ")")
    # This context should have been passed to translate_stata_expression_with_r_values for r_expr if needed.
  }

  # For replace, the structure is: var = ifelse(condition, new_value, old_value)
  # If no `if` condition, it's just: var = new_value
  if (!is.na(r_if_cond) && r_if_cond != "") {
    mutate_expr = paste0(var_to_replace, " = dplyr::if_else(", r_if_cond, ", ", r_expr, ", ", var_to_replace, ")")
  } else {
    mutate_expr = paste0(var_to_replace, " = ", r_expr)
  }

  if (!is.null(by_vars_r)) {
    # `replace` with `by` prefix: `by group: replace x = x[_n-1] if _n > 1`
    # This means the expression `x[_n-1]` and condition `_n > 1` are evaluated within groups.
    # The context passed to translate_stata_expression_with_r_values should reflect this.
    # dplyr:
    r_code_str = paste0("data = data %>%\n  dplyr::group_by(", gsub("c\\(|\\)", "", by_vars_r), ") %>%\n  dplyr::mutate(", mutate_expr, ") %>%\n  dplyr::ungroup()")
    # collapse:
    # r_code_str = paste0("data = collapse::fgroup_by(data, ", by_vars_r, ")")
    # r_code_str = paste0(r_code_str, "\ndata = collapse::fmutate(data, ", mutate_expr, ")") # add=FALSE by default, so it replaces
    # r_code_str = paste0(r_code_str, "\ndata = collapse::fungroup(data)")
  } else {
    # dplyr:
    r_code_str = paste0("data = dplyr::mutate(data, ", mutate_expr, ")")
    # collapse:
    # r_code_str = paste0("data = collapse::fmutate(data, ", mutate_expr, ")") # add=FALSE by default
  }

  return(r_code_str)
}

