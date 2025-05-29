t_replace = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_replace")
  # Capture explicit type declaration (e.g., `str10`)
  explicit_type_match = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*(byte|int|long|float|double|str\\d+|strL)\\s+")
  declared_type_str = NA_character_
  if (!is.na(explicit_type_match[1,1])) {
    declared_type_str = explicit_type_match[1,2]
  }

  # Strip type if present (e.g. replace double oldvar = ...)
  rest_of_cmd_no_type = stringi::stri_replace_first_regex(rest_of_cmd, "^(?:byte|int|long|float|double|str\\d+|strL)\\s+", "")

  match = stringi::stri_match_first_regex(rest_of_cmd_no_type, "^\\s*([^=\\s]+)\\s*=\\s*(.*?)(?:\\s+if\\s+(.*))?$")

  if (is.na(match[1,1])) {
    return(paste0("# Failed to parse replace command: ", rest_of_cmd))
  }

  var_to_replace = stringi::stri_trim_both(match[1,2])
  stata_expr = stringi::stri_trim_both(match[1,3])
  stata_if_cond = stringi::stri_trim_both(match[1,4]) # Might be NA

  current_context = list(is_by_group = cmd_obj$is_by_prefix && length(cmd_obj$by_group_vars) > 0 && !is.na(cmd_obj$by_group_vars[1]))
  # Translate the Stata expression to R first
  r_expr = translate_stata_expression_with_r_values(stata_expr, line_num, cmd_df, current_context)

  r_if_cond = NA_character_
  if (!is.na(stata_if_cond) && stata_if_cond != "") {
    r_if_cond = translate_stata_expression_with_r_values(stata_if_cond, line_num, cmd_df, context = list(is_by_group = FALSE))
  }

  # Determine arrange step if needed
  arrange_call = ""
  group_vars_list_bare = character(0) # For dplyr::group_by

  # Variables that define the sort order (from by-prefix)
  vars_for_initial_sort = character(0)

  if (cmd_obj$is_by_prefix) {
    if (length(cmd_obj$by_group_vars) > 0 && !is.na(cmd_obj$by_group_vars[1])) {
      group_vars_list = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
      group_vars_list = group_vars_list[!is.na(group_vars_list) & group_vars_list != ""]
      if (length(group_vars_list) > 0) {
        group_vars_list_bare = group_vars_list # Assign just the bare names
        vars_for_initial_sort = c(vars_for_initial_sort, group_vars_list)
      }
    }

    if (length(cmd_obj$by_sort_vars) > 0 && !is.na(cmd_obj$by_sort_vars[1])) {
      sort_vars_list = stringi::stri_split_fixed(cmd_obj$by_sort_vars, ",")[[1]]
      sort_vars_list = sort_vars_list[!is.na(sort_vars_list) & sort_vars_list != ""]
      vars_for_initial_sort = c(vars_for_initial_sort, sort_vars_list)
    }

    if (length(vars_for_initial_sort) > 0) {
      vars_for_initial_sort = unique(vars_for_initial_sort)
      arrange_call = paste0("data = dplyr::arrange(data, !!!dplyr::syms(c(", paste0('"', vars_for_initial_sort, '"', collapse = ", "), ")))")
    }
  }

  # Determine if the target variable will be a string type based on the R expression.
  target_var_will_be_string = FALSE
  if (!is.na(declared_type_str) && stringi::stri_startswith_fixed(declared_type_str, "str")) {
      target_var_will_be_string = TRUE
  } else {
      # Infer type from Stata expression if not explicitly declared
      target_var_will_be_string = sfun_is_stata_expression_string_typed(stata_expr)
  }
  # If not explicitly declared and not string expression, assume numeric.

  # Step 1: Calculate the value for the new variable, potentially conditionally
  calculated_value_expr_raw = r_expr # This is the R translation of stata_expr

  # Determine if `calculated_value_expr_raw` (the R expression) is a logical one (TRUE/FALSE result).
  # This is a heuristic. Exclude `dplyr::if_else` because its output type is managed internally
  # and it already handles type consistency for its arguments.
  is_logical_r_expr = stringi::stri_detect_regex(calculated_value_expr_raw, "\\bTRUE\\b|\\bFALSE\\b|==|!=|<=|>=|<|>|&|\\|") &&
                      !stringi::stri_detect_fixed(calculated_value_expr_raw, "dplyr::if_else")

  if (target_var_will_be_string) {
      # If Stata expression is numeric NA (.), it translates to NA_real_.
      # When assigned to a string variable, Stata treats '.' as "".
      if (calculated_value_expr_raw == "NA_real_") {
          calculated_value_expr = '""'
      } else if (stringi::stri_startswith_fixed(calculated_value_expr_raw, '"') || stringi::stri_startswith_fixed(calculated_value_expr_raw, "'")) {
          # Already a string literal, no casting needed (assuming it's correctly translated)
          calculated_value_expr = calculated_value_expr_raw
      } else {
          # Cast to character for other expressions (e.g., numeric variables, logicals)
          calculated_value_expr = paste0("as.character(", calculated_value_expr_raw, ")")
      }
  } else { # Target variable will be numeric/logical
      if (is_logical_r_expr) {
          # Logical expressions should result in numeric 0/1 (Stata's behavior)
          calculated_value_expr = paste0("as.numeric(", calculated_value_expr_raw, ")")
      } else {
          # Default to the raw translated expression (should be numeric)
          calculated_value_expr = calculated_value_expr_raw
      }
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
  }

  pipe_elements = list("data") # Elements for the pipe chain, starting from `data`

  if (length(group_vars_list_bare) > 0) { # Check if group_vars_list_bare is not empty
      group_by_call_str = paste0('dplyr::group_by(!!!dplyr::syms(c("', paste0(group_vars_list_bare, collapse='", "'), '")))')
      pipe_elements = c(pipe_elements, group_by_call_str)
      pipe_elements = c(pipe_elements, paste0("dplyr::mutate(`", var_to_replace, "` = ", calc_expr, ")"))
      pipe_elements = c(pipe_elements, "dplyr::ungroup()")
  } else {
      pipe_elements = c(pipe_elements, paste0("dplyr::mutate(`", var_to_replace, "` = ", calc_expr, ")"))
  }

  r_code_lines = c(r_code_lines, paste0("data = ", paste(pipe_elements, collapse = " %>% \n  ")))

  return(paste(r_code_lines, collapse="\n"))
}

