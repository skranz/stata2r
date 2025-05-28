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

  current_context = list(is_by_group = cmd_obj$is_by_prefix && length(cmd_obj$by_group_vars) > 0 && !is.na(cmd_obj$by_group_vars[1]))
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
    if (length(cmd_obj$by_group_vars) > 0 && !is.na(cmd_obj$by_group_vars[1])) {
      group_vars_list = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
      # Ensure group_vars_list is clean (no NA or empty strings)
      group_vars_list = group_vars_list[!is.na(group_vars_list) & group_vars_list != ""]
      if (length(group_vars_list) > 0) { # Ensure group_vars_list is not empty before forming string
        group_vars_r_vec_str = paste0('dplyr::all_of(c("', paste0(group_vars_list, collapse='", "'), '"))')
      }
    }

    sort_vars_list = character(0)
    if (length(cmd_obj$by_sort_vars) > 0 && !is.na(cmd_obj$by_sort_vars[1])) {
      sort_vars_list = stringi::stri_split_fixed(cmd_obj$by_sort_vars, ",")[[1]]
      # Ensure sort_vars_list is clean (no NA or empty strings)
      sort_vars_list = sort_vars_list[!is.na(sort_vars_list) & sort_vars_list != ""]
    }

    if (length(sort_vars_list) > 0) {
      all_sort_vars = c(if(length(group_vars_list)>0) group_vars_list else character(0), sort_vars_list)
      all_sort_vars = all_sort_vars[!is.na(all_sort_vars) & all_sort_vars != ""] # Final clean
      if (length(all_sort_vars) > 0) {
        arrange_call = paste0("data = dplyr::arrange(data, dplyr::across(dplyr::all_of(c(", paste0('"', all_sort_vars, '"', collapse = ", "), "))))")
      }
    }
  }

  # Step 1: Calculate the value for replacement, potentially conditionally
  # Ensure logical comparisons are converted to numeric (0/1) to match Stata's default numeric type for logical expressions.
  is_logical_expr = stringi::stri_detect_regex(stata_expr, "==|!=|~=|<=|>=|<|>|&|\\|")
  is_string_result_type = is_stata_expr_string_type(stata_expr)

  calculated_value_expr = r_expr
  na_for_if_else = "NA_real_" # Default to numeric NA

  if (is_logical_expr) {
    calculated_value_expr = paste0("as.numeric(", r_expr, ")")
    na_for_if_else = "NA_real_"
  } else if (is_string_result_type) {
    na_for_if_else = "NA_character_"
  }

  # For 'replace' command, if condition is FALSE or NA, the value should be left unchanged.
  # Use dplyr::coalesce(condition, FALSE) to treat NA condition as FALSE.
  if (!is.na(r_if_cond) && r_if_cond != "") {
    calc_expr = paste0("dplyr::if_else(dplyr::coalesce(", r_if_cond, ", FALSE), ", calculated_value_expr, ", data$`", var_to_replace, "`)")
  } else {
    calc_expr = calculated_value_expr
  }

  # Step 2: Build the R code string using pipes for the mutate operation
  r_code_lines = c()

  if (arrange_call != "") {
      r_code_lines = c(r_code_lines, arrange_call)
      r_code_lines = c(r_code_lines, "data = data %>%")
  } else {
      r_code_lines = c(r_code_lines, "data = data %>%")
  }

  if (!is.null(group_vars_r_vec_str)) { # Check if group_vars_r_vec_str is not NULL
      r_code_lines = c(r_code_lines, paste0("  dplyr::group_by(", group_vars_r_vec_str, ") %>%"))
      r_code_lines = c(r_code_lines, paste0("  dplyr::mutate(`", var_to_replace, "` = ", calc_expr, ") %>%"))
      r_code_lines = c(r_code_lines, "  dplyr::ungroup()")
  } else {
      r_code_lines = c(r_code_lines, paste0("  dplyr::mutate(`", var_to_replace, "` = ", calc_expr, ")"))
  }

  return(paste(r_code_lines, collapse="\n"))
}


