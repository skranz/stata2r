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
  current_context = list(is_by_group = cmd_obj$is_by_prefix && !is.na(cmd_obj$by_group_vars))
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

    # If there are sort keys for by-processing, prepare the arrange call
    if (length(sort_vars_list) > 0) {
      all_sort_vars = c(if(length(group_vars_list)>0) group_vars_list else character(0), sort_vars_list)
      all_sort_vars_str = paste(all_sort_vars, collapse = ", ")
      arrange_call = paste0("dplyr::arrange(data, ", all_sort_vars_str, ")")
    }
  }

  # Step 1: Calculate the value for the new variable, potentially conditionally
  # Do not strip attributes here, let mutate propagate them, then strip from the final column
  if (!is.na(r_if_cond) && r_if_cond != "") {
    calc_expr = paste0("dplyr::if_else(", r_if_cond, ", ", r_expr, ", NA)")
  } else {
    calc_expr = r_expr
  }
  
  # Step 2: Build the R code string using pipes for the mutate operation
  r_code_lines = c()

  # Start with data or data after arrange, and add the first pipe
  if (arrange_call != "") {
      r_code_lines = c(r_code_lines, paste0("data = ", arrange_call, " %>%\n"))
  } else {
      r_code_lines = c(r_code_lines, "data = data %>%\n")
  }

  # Add grouping and mutate steps
  if (!is.null(group_vars_r_vec_str) && length(group_vars_list) > 0) {
      r_code_lines = c(r_code_lines, paste0("  dplyr::group_by(dplyr::across(", group_vars_r_vec_str, ")) %>%\n"))
      r_code_lines = c(r_code_lines, paste0("  dplyr::mutate(", new_var, " = ", calc_expr, ") %>%\n"))
      r_code_lines = c(r_code_lines, "  dplyr::ungroup()")
  } else {
      # If not grouped, just add the mutate step directly to the pipe chain
      r_code_lines = c(r_code_lines, paste0("  dplyr::mutate(", new_var, " = ", calc_expr, ")"))
  }
  
  # Step 3: Apply Stata-like numeric output rounding for precision matching
  r_code_lines = c(r_code_lines, paste0("data$", new_var, " = sfun_stata_numeric_output_round(data$", new_var, ")"))

  # Step 4: Strip Stata-specific attributes from the newly created/modified column
  r_code_lines = c(r_code_lines, paste0("data$", new_var, " = sfun_strip_stata_attributes(data$", new_var, ")"))
  
  return(paste(r_code_lines, collapse="\n"))
}

