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

  current_context = list(is_by_group = cmd_obj$is_by_prefix && !is.na(cmd_obj$by_group_vars))
  r_expr = translate_stata_expression_with_r_values(stata_expr, line_num, cmd_df, current_context)

  r_if_cond = NA_character_
  if (!is.na(stata_if_cond) && stata_if_cond != "") {
    r_if_cond = translate_stata_expression_with_r_values(stata_if_cond, line_num, cmd_df, context = list(is_by_group = FALSE))
  }

  # Determine arrange step if needed
  arrange_call = ""
  group_vars_r_vec_str = NULL

  if (cmd_obj$is_by_prefix) {
    if (!is.na(cmd_obj$by_group_vars)) {
      group_vars_list = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
      group_vars_list = group_vars_list[group_vars_list != ""]
      group_vars_r_vec_str = paste0('c("', paste0(group_vars_list, collapse='", "'), '")')
    }

    sort_vars_list = character(0)
    if (!is.na(cmd_obj$by_sort_vars)) {
      sort_vars_list = stringi::stri_split_fixed(cmd_obj$by_sort_vars, ",")[[1]]
      sort_vars_list = sort_vars_list[sort_vars_list != ""]
    }

    if (length(sort_vars_list) > 0) {
      all_sort_vars = c(if(!is.null(group_vars_list)) group_vars_list else character(0), sort_vars_list)
      all_sort_vars_str = paste(all_sort_vars, collapse = ", ")
      arrange_call = paste0("dplyr::arrange(data, ", all_sort_vars_str, ")")
    }
  }


  if (!is.na(r_if_cond) && r_if_cond != "") {
    mutate_expr = paste0(var_to_replace, " = dplyr::if_else(", r_if_cond, ", ", r_expr, ", ", var_to_replace, ")")
  } else {
    mutate_expr = paste0(var_to_replace, " = ", r_expr)
  }

  # Build the R code string using pipes
  r_code_str = "data = "

  if (arrange_call != "") {
      r_code_str = paste0(r_code_str, arrange_call)
  } else {
      r_code_str = paste0(r_code_str, "data")
  }

  if (!is.null(group_vars_r_vec_str)) {
      r_code_str = paste0(r_code_str, " %>%\n  collapse::fgroup_by(", group_vars_r_vec_str, ")")
      r_code_str = paste0(r_code_str, " %>%\n  collapse::fmutate(", mutate_expr, ")")
      r_code_str = paste0(r_code_str, " %>%\n  collapse::fungroup()")
  } else {
      r_code_str = paste0(r_code_str, " %>%\n  collapse::fmutate(", mutate_expr, ")")
  }

  return(r_code_str)
}

