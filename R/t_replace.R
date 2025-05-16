# Translate Stata 'replace' command
# Stata: replace oldvar = expression [if condition]
t_replace = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
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
    r_if_cond = translate_stata_expression_with_r_values(stata_if_cond, line_num, cmd_df, context = list(is_by_group = FALSE))
  }

  by_vars_r_vec_str = NULL
  if (cmd_obj$is_by_prefix && !is.na(cmd_obj$by_vars)) {
    by_vars_list = stringi::stri_split_fixed(cmd_obj$by_vars, " ")[[1]]
    by_vars_list = by_vars_list[by_vars_list != ""]
    by_vars_r_vec_str = paste0('c("', paste0(by_vars_list, collapse='", "'), '")')
  }

  # For replace, the structure is: var = ifelse(condition, new_value, old_value)
  # dplyr::if_else is type-strict. Base ifelse is more flexible.
  if (!is.na(r_if_cond) && r_if_cond != "") {
    # Using dplyr::if_else, assuming type of r_expr and var_to_replace are compatible.
    mutate_expr = paste0(var_to_replace, " = dplyr::if_else(", r_if_cond, ", ", r_expr, ", ", var_to_replace, ")")
    # Alternative with base ifelse:
    # mutate_expr = paste0(var_to_replace, " = ifelse(", r_if_cond, ", ", r_expr, ", ", var_to_replace, ")")
  } else {
    mutate_expr = paste0(var_to_replace, " = ", r_expr)
  }

  if (!is.null(by_vars_r_vec_str)) {
    # Using collapse for grouped mutation:
    r_code_str = paste0("data = collapse::fgroup_by(data, ", by_vars_r_vec_str, ")")
    r_code_str = paste0(r_code_str, "\ndata = collapse::fmutate(data, ", mutate_expr, ")") # fmutate replaces existing var
    r_code_str = paste0(r_code_str, "\ndata = collapse::fungroup(data)")
    # dplyr alternative:
    # by_vars_dplyr_str = gsub('c\\("', '', gsub('"\\)', '', gsub('", "', ',', by_vars_r_vec_str)))
    # r_code_str = paste0("data = data %>%\n  dplyr::group_by(", by_vars_dplyr_str, ") %>%\n  dplyr::mutate(", mutate_expr, ") %>%\n  dplyr::ungroup()")
  } else {
    # Using collapse:
    r_code_str = paste0("data = collapse::fmutate(data, ", mutate_expr, ")")
    # dplyr alternative:
    # r_code_str = paste0("data = dplyr::mutate(data, ", mutate_expr, ")")
  }

  return(r_code_str)
}


