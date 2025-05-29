t_generate = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_generate")
  # Parse `rest_of_cmd` for new variable name, expression, and if condition
  # Example: "newvar = expression [if condition]"

  # Capture explicit type declaration (e.g., `str10`)
  explicit_type_match = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*(byte|int|long|float|double|str\\d+|strL)\\s+")
  declared_type_str = NA_character_
  if (!is.na(explicit_type_match[1,1])) {
    declared_type_str = explicit_type_match[1,2]
  }

  # Strip type if present (e.g. gen double newvar = ...)
  rest_of_cmd_no_type = stringi::stri_replace_first_regex(rest_of_cmd, "^\\s*(?:byte|int|long|float|double|str\\d+|strL)\\s+", "")

  match = stringi::stri_match_first_regex(rest_of_cmd_no_type, "^\\s*([^=\\s]+)\\s*=\\s*(.*?)(?:\\s+if\\s+(.*))?$")

  # NEW: Defensive check for successful parsing of core components
  if (is.na(match[1,1]) || is.na(match[1,2]) || is.na(match[1,3])) {
    return(paste0("# Failed to parse generate command structure: ", rest_of_cmd))
  }

  new_var = stringi::stri_trim_both(match[1,2])
  stata_expr = stringi::stri_trim_both(match[1,3])
  stata_if_cond = stringi::stri_trim_both(match[1,4]) # Might be NA

  # Context for expression translation (e.g. _n, _N behavior)
  # is_by_group TRUE if cmd_obj$by_group_vars is not NA
  current_context = list(is_by_group = cmd_obj$is_by_prefix && length(cmd_obj$by_group_vars) > 0 && !is.na(cmd_obj$by_group_vars[1]))
  
  # Translate the Stata expression to R first
  r_expr = translate_stata_expression_with_r_values(stata_expr, line_num, cmd_df, current_context)

  # Ensure r_expr is a character string literal, even if it represents NA (logical)
  if (is.na(r_expr) && !is.character(r_expr)) { # Check for logical NA, not string "NA"
      r_expr = "NA_real_"
  } else if (is.character(r_expr) && r_expr == "") {
      # An empty string translated expression should likely be NA in numeric context
      # or empty string in string context. Let's make it explicit for safety.
      r_expr = "NA_real_" # Default to NA_real_ if it's empty string after translation
  }


  r_if_cond = NA_character_
  if (!is.na(stata_if_cond) && stata_if_cond != "") {
    # The 'if' condition for generate/replace is evaluated row-wise on the whole dataset, not per group.
    r_if_cond = translate_stata_expression_with_r_values(stata_if_cond, line_num, cmd_df, context = list(is_by_group = FALSE))
  }

  # Determine group_vars for dplyr::group_by
  group_vars_list_bare = character(0) 
  
  if (cmd_obj$is_by_prefix) {
    if (length(cmd_obj$by_group_vars) > 0 && !is.na(cmd_obj$by_group_vars[1])) {
      group_vars_list = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
      group_vars_list = group_vars_list[!is.na(group_vars_list) & group_vars_list != ""]
      if (length(group_vars_list) > 0) {
        group_vars_list_bare = group_vars_list # Assign just the bare names
      }
    }
  }

  # Determine if the target variable will be a string type based on EXPLICIT Stata declaration.
  # If no explicit type, let R infer from the translated expression.
  force_r_output_type = NA_character_ # "character" or "numeric"

  # FIX: Added !is.na(declared_type_str) to prevent error if declared_type_str is NA
  if (!is.na(declared_type_str)) {
      if (stringi::stri_startswith_fixed(declared_type_str, "str")) {
          force_r_output_type = "character"
      } else { # byte, int, long, float, double
          force_r_output_type = "numeric"
      }
  }

  # Step 1: Calculate the value for the new variable, potentially conditionally
  calculated_value_expr_raw = r_expr # This is the R translation of stata_expr

  # Apply explicit type casting if declared in Stata command
  if (force_r_output_type == "character") {
      # If Stata expression is numeric NA (.), it translates to NA_real_.
      # When assigned to a string variable, Stata treats '.' as "".
      if (isTRUE(calculated_value_expr_raw == "NA_real_")) {
          calculated_value_expr = '""'
      } else {
          # Cast to character for other expressions
          calculated_value_expr = paste0("as.character(", calculated_value_expr_raw, ")")
      }
  } else if (force_r_output_type == "numeric") {
      # Ensure logicals become 0/1. Other numeric types should already be fine.
      # This handles `gen newvar = x==y` resulting in numeric 0/1.
      # Added robustness check for NA or empty `calculated_value_expr_raw`
      is_logical_r_expr = FALSE # Default to FALSE
      if (!is.na(calculated_value_expr_raw) && calculated_value_expr_raw != "") {
        # Check if the expression contains logical operators or literals, and is not already an if_else.
        regex_match = stringi::stri_detect_regex(calculated_value_expr_raw, "\\bTRUE\\b|\\bFALSE\\b|==|!=|<=|>=|<|>|&|\\||\\bsfun_missing\\b")
        fixed_match = stringi::stri_detect_fixed(calculated_value_expr_raw, "dplyr::if_else")
        # Ensure the result of logical operations is always TRUE/FALSE, never NA.
        is_logical_r_expr = dplyr::coalesce(regex_match, FALSE) && !dplyr::coalesce(fixed_match, FALSE)
      }
      if (is_logical_r_expr) {
          calculated_value_expr = paste0("as.numeric(", calculated_value_expr_raw, ")")
      } else {
          calculated_value_expr = calculated_value_expr_raw
      }
  } else { # No explicit type declared in Stata, let R infer
      calculated_value_expr = calculated_value_expr_raw
  }

  # The value to assign if the condition is false/missing.
  # If R infers, it should infer NA for missing.
  na_or_empty_str_for_false_cond = if (sfun_is_stata_expression_string_typed(stata_expr)) '""' else "NA_real_"


  # Apply condition only if it exists
  # Introduce temporary variable for condition evaluation to avoid translation-time logical error
  temp_if_cond_var = paste0("stata_tmp_if_cond_L", line_num)
  
  r_code_lines = c()
  
  # Generate the condition variable first using with(data, ...) to evaluate at runtime
  if (!is.na(r_if_cond) && r_if_cond != "") {
      r_code_lines = c(r_code_lines, paste0(temp_if_cond_var, " = as.logical(dplyr::coalesce(with(data, ", r_if_cond, "), FALSE))"))
  } else {
      r_code_lines = c(r_code_lines, paste0(temp_if_cond_var, " = TRUE")) # Always apply if no condition
  }


  if (!is.na(r_if_cond) && r_if_cond != "") {
    calc_expr = paste0("dplyr::if_else(", temp_if_cond_var, ", ", calculated_value_expr, ", ", na_or_empty_str_for_false_cond, ")")
  } else {
    calc_expr = calculated_value_expr
  }

  # Step 2: Build the R code string using pipes for the mutate operation
  pipe_elements = list("data") # Elements for the pipe chain, starting from `data`

  # Add grouping and mutate steps
  if (length(group_vars_list_bare) > 0) {
      group_by_call_str = paste0('dplyr::group_by(!!!dplyr::syms(c("', paste0(group_vars_list_bare, collapse='", "'), '")))')
      pipe_elements = c(pipe_elements, group_by_call_str)
      pipe_elements = c(pipe_elements, paste0("dplyr::mutate(`", new_var, "` = ", calc_expr, ")"))
      pipe_elements = c(pipe_elements, "dplyr::ungroup()")
  } else {
      pipe_elements = c(pipe_elements, paste0("dplyr::mutate(`", new_var, "` = ", calc_expr, ")"))
  }

  # This is the final assignment line for the current command
  # It takes 'data' (potentially already arranged) and pipes it through the rest
  r_code_lines = c(r_code_lines, paste0("data = ", paste(pipe_elements, collapse = " %>% \n  ")))
  
  # Clean up temporary condition variable
  r_code_lines = c(r_code_lines, paste0("rm(", temp_if_cond_var, ")"))


  return(paste(r_code_lines, collapse="\n"))
}

