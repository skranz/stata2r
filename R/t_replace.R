t_replace = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_replace")
  # Strip type if present (e.g. replace double oldvar = ...)
  rest_of_cmd_no_type = stringi::stri_replace_first_regex(rest_of_cmd, "^(?:byte|int|long|float|double|str\\d+)\\s+", "")

  match = stringi::stri_match_first_regex(rest_of_cmd_no_type, "^\\s*([^=\\s]+)\\s*=\\s*(.*?)(?:\\s+if\\s+(.*))?$")

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
  group_vars_list = character(0) # Initialize for use in all_sort_vars

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
      all_sort_vars = c(if(length(group_vars_list)>0) group_vars_list else character(0), sort_vars_list)
      all_sort_vars_str = paste(all_sort_vars, collapse = ", ")
      arrange_call = paste0("dplyr::arrange(data, ", all_sort_vars_str, ")")
    }
  }

  # Step 1: Calculate the value for replacement, potentially conditionally
  # Do not strip attributes here, let mutate propagate them, then strip from the final column
  if (!is.na(r_if_cond) && r_if_cond != "") {
    calc_expr = paste0("dplyr::if_else(", r_if_cond, ", ", r_expr, ", ", var_to_replace, ")")
  } else {
    calc_expr = r_expr
  }

  # Step 2: Build the R code string using pipes for the mutate operation
  r_code_lines = c()

  if (arrange_call != "") {
      r_code_lines = c(r_code_lines, paste0("data = ", arrange_call, " %>%\n"))
  } else {
      r_code_lines = c(r_code_lines, "data = data %>%\n")
  }

  if (!is.null(group_vars_r_vec_str) && length(group_vars_list) > 0) {
      r_code_lines = c(r_code_lines, paste0("  dplyr::group_by(dplyr::across(", group_vars_r_vec_str, ")) %>%\n"))
      r_code_lines = c(r_code_lines, paste0("  dplyr::mutate(", var_to_replace, " = ", calc_expr, ") %>%\n"))
      r_code_lines = c(r_code_lines, "  dplyr::ungroup()")
  } else {
      r_code_lines = c(r_code_lines, paste0("  dplyr::mutate(", var_to_replace, " = ", calc_expr, ")"))
  }

  # Step 3: Strip Stata-specific attributes from the modified column
  r_code_lines = c(r_code_lines, paste0("data$", var_to_replace, " = sfun_strip_stata_attributes(data$", var_to_replace, ")"))

  return(paste(r_code_lines, collapse="\n"))
}

