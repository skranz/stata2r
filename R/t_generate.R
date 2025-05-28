t_generate = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_generate")
  # Parse `rest_of_cmd` for new variable name, expression, and if condition
  # Example: "newvar = expression [if condition]"

  # Strip type if present (e.g. gen double newvar = ...)
  rest_of_cmd_no_type = stringi::stri_replace_first_regex(rest_of_cmd, "^(?:byte|int|long|float|double|str\\d+)\\s+", "")

  match = stringi::stri_match_first_regex(rest_of_cmd_no_type, "^\\s*([^=\\s]+)\\s*=\\s*(.*?)(?:\\s+if\\s+(.*))?$")

  if (is.na(match[1,1])) {
    return(paste0("# Failed to parse generate command: ", rest_of_cmd))
  }

  new_var = stringi::stri_trim_both(match[1,2])
  stata_expr = stringi::stri_trim_both(match[1,3])
  stata_if_cond = stringi::stri_trim_both(match[1,4]) # Might be NA

  # Context for expression translation (e.g. _n, _N behavior)
  # is_by_group TRUE if cmd_obj$by_group_vars is not NA
  current_context = list(is_by_group = cmd_obj$is_by_prefix && length(cmd_obj$by_group_vars) > 0 && !is.na(cmd_obj$by_group_vars[1]))
  r_expr = translate_stata_expression_with_r_values(stata_expr, line_num, cmd_df, current_context)

  r_if_cond = NA_character_
  if (!is.na(stata_if_cond) && stata_if_cond != "") {
    # The 'if' condition for generate/replace is evaluated row-wise on the whole dataset, not per group.
    r_if_cond = translate_stata_expression_with_r_values(stata_if_cond, line_num, cmd_df, context = list(is_by_group = FALSE))
  }

  # Determine arrange step if needed
  arrange_call = ""
  group_vars_r_vec_str = NULL
  group_vars_list = character(0) # Initialize for use in all_sort_vars

  if (cmd_obj$is_by_prefix) {
    if (length(cmd_obj$by_group_vars) > 0 && !is.na(cmd_obj$by_group_vars[1])) {
      group_vars_list = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
      group_vars_list = group_vars_list[group_vars_list != ""]
      group_vars_r_vec_str = paste0('c("', paste0(group_vars_list, collapse='", "'), '")')
    }

    sort_vars_list = character(0)
    if (length(cmd_obj$by_sort_vars) > 0 && !is.na(cmd_obj$by_sort_vars[1])) {
      sort_vars_list = stringi::stri_split_fixed(cmd_obj$by_sort_vars, ",")[[1]]
      sort_vars_list = sort_vars_list[sort_vars_list != ""]
    }

    # If there are sort keys for by-processing, prepare the arrange call
    if (length(sort_vars_list) > 0) {
      all_sort_vars = c(if(length(group_vars_list)>0) group_vars_list else character(0), sort_vars_list)
      all_sort_vars_str = paste(all_sort_vars, collapse = ", ")
      arrange_call = paste0("data = dplyr::arrange(data, ", all_sort_vars_str, ")")
    }
  }

  # Step 1: Calculate the value for the new variable, potentially conditionally
  # Ensure logical comparisons are converted to numeric (0/1) to match Stata's default numeric type for logical expressions.
  is_logical_expr = stringi::stri_detect_regex(stata_expr, "==|!=|~=|<=|>=|<|>|&|\\|")
  is_string_result_type = is_stata_expr_string_type(stata_expr)

  calculated_value_expr = r_expr
  na_for_if_else = "NA_real_" # Default to numeric NA

  if (is_logical_expr) {
    calculated_value_expr = paste0("as.numeric(", r_expr, ")")
    # If the expression itself is logical, Stata converts it to 0/1. So the result is numeric.
    # Hence, NA_real_ is appropriate.
    na_for_if_else = "NA_real_"
  } else if (is_string_result_type) {
    # If not a logical expression, but identified as producing a string result, use NA_character_.
    na_for_if_else = "NA_character_"
  }
  # Otherwise (non-logical, non-string, i.e., numeric expression), NA_real_ is already the default.


  if (!is.na(r_if_cond) && r_if_cond != "") {
    # Stata's 'if' condition treats NA as FALSE.
    calc_expr = paste0("dplyr::if_else(dplyr::coalesce(", r_if_cond, ", FALSE), ", calculated_value_expr, ", ", na_for_if_else, ")")
  } else {
    calc_expr = calculated_value_expr
  }

  # Step 2: Build the R code string using pipes for the mutate operation
  r_code_lines = c()

  # Start with data or data after arrange, and add the first pipe
  if (arrange_call != "") {
      r_code_lines = c(r_code_lines, arrange_call)
      r_code_lines = c(r_code_lines, "data = data %>%")
  } else {
      r_code_lines = c(r_code_lines, "data = data %>%")
  }

  # Add grouping and mutate steps
  if (!is.null(group_vars_r_vec_str) && length(group_vars_list) > 0) {
      r_code_lines = c(r_code_lines, paste0("  dplyr::group_by(dplyr::across(", group_vars_r_vec_str, ")) %>%"))
      r_code_lines = c(r_code_lines, paste0("  dplyr::mutate(", new_var, " = ", calc_expr, ") %>%"))
      r_code_lines = c(r_code_lines, "  dplyr::ungroup()")
  } else {
      # If not grouped, just add the mutate step directly to the pipe chain
      r_code_lines = c(r_code_lines, paste0("  dplyr::mutate(", new_var, " = ", calc_expr, ")"))
  }


  return(paste(r_code_lines, collapse="\n"))
}

