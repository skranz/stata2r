t_replace = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_replace")
  # Capture explicit type declaration (e.g., `str10`)
  explicit_type_match = stringi::stri_match_first_regex(rest_of_cmd, "^(?:byte|int|long|float|double|str\\d+|strL)\\s+")
  declared_type_str = NA_character_
  if (!is.na(explicit_type_match[1,1])) {
    declared_type_str = explicit_type_match[1,2]
  }

  # Strip type if present (e.g. replace double oldvar = ...)
  rest_of_cmd_no_type = stringi::stri_replace_first_regex(rest_of_cmd, "^(?:byte|int|long|float|double|str\\d+|strL)\\s+", "")

  match = stringi::stri_match_first_regex(rest_of_cmd_no_type, "^\\s*([^=\\s]+)\\s*=\\s*(.*?)(?:\\s+if\\s+(.*))?$")

  # Defensive assignment for potentially NA match groups
  var_to_replace = NA_character_
  stata_expr = NA_character_
  stata_if_cond = NA_character_

  if (!is.na(match[1,1])) { # If the overall regex matched
      var_to_replace = dplyr::coalesce(stringi::stri_trim_both(match[1,2]), NA_character_)
      stata_expr = dplyr::coalesce(stringi::stri_trim_both(match[1,3]), NA_character_)
      stata_if_cond = dplyr::coalesce(stringi::stri_trim_both(match[1,4]), NA_character_)
  } else {
      # If no match, return a parsing error
      return(paste0("# Failed to parse replace command structure: ", rest_of_cmd))
  }


  current_context = list(is_by_group = cmd_obj$is_by_prefix && length(cmd_obj$by_group_vars) > 0 && !is.na(cmd_obj$by_group_vars[1]))
  # Translate the Stata expression to R first
  r_expr = translate_stata_expression_with_r_values(stata_expr, line_num, cmd_df, current_context)

  # Ensure r_expr is a character string literal, even if it represents NA (logical)
  if (is.na(r_expr)) { # Check for logical NA, not string "NA"
      r_expr = "NA_real_"
  }

  r_if_cond = NA_character_
  if (!is.na(stata_if_cond) && stata_if_cond != "") {
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
  is_stata_expr_string_typed = sfun_is_stata_expression_string_typed(stata_expr)

  # Apply explicit type casting if declared in Stata command, overriding inferred type
  if (!is.na(declared_type_str)) {
      if (stringi::stri_startswith_fixed(declared_type_str, "str")) {
          is_stata_expr_string_typed = TRUE
      } else { # byte, int, long, float, double
          is_stata_expr_string_typed = FALSE
      }
  }

  # Step 1: Calculate the value for the new variable, potentially conditionally
  calculated_value_expr_raw = r_expr # This is the R translation of stata_expr

  if (is_stata_expr_string_typed) {
      # If Stata expression is numeric NA (.), it translates to NA_real_.
      # When assigned to a string variable, Stata treats '.' as "".
      if (calculated_value_expr_raw == "NA_real_") { # Check against the literal string "NA_real_"
          calculated_value_expr = '""'
      } else {
          calculated_value_expr = paste0("as.character(", calculated_value_expr_raw, ")")
      }
  } else { # Numeric output
      # Ensure logicals become 0/1. Stata converts TRUE/FALSE to 1/0 for numeric types.
      # This handles `gen newvar = x==y` resulting in numeric 0/1.
      # If it's a logical expression, cast to numeric.
      is_a_logical_expression = isTRUE(
          stringi::stri_detect_regex(calculated_value_expr_raw, "\\bTRUE\\b|\\bFALSE\\b|==|!=|<=|>=|<|>|&|\\||\\bsfun_missing\\b") &&
          !stringi::stri_detect_fixed(calculated_value_expr_raw, "dplyr::if_else")
      )

      if (is_a_logical_expression) {
          calculated_value_expr = paste0("as.numeric(", calculated_value_expr_raw, ")")
      } else {
          calculated_value_expr = calculated_value_expr_raw
      }
  }


  # For 'replace' command, if condition is FALSE or NA, the value should be left unchanged.
  # Use dplyr::coalesce(condition, FALSE) to treat NA condition as FALSE.
  # Apply condition only if it exists
  if (!is.na(r_if_cond) && r_if_cond != "") {
    calc_expr = paste0("dplyr::if_else(as.logical(dplyr::coalesce(", r_if_cond, ", FALSE)), ", calculated_value_expr, ", data$`", var_to_replace, "`)")
  } else {
    calc_expr = calculated_value_expr
  }

  # Step 2: Build the R code string using pipes for the mutate operation
  pipe_elements = list("data") # Elements for the pipe chain, starting from `data`

  if (length(group_vars_list_bare) > 0) { # Check if group_vars_list_bare is not empty
      group_by_call_str = paste0('dplyr::group_by(!!!dplyr::syms(c("', paste0(group_vars_list_bare, collapse='", "'), '")))')
      pipe_elements = c(pipe_elements, group_by_call_str)
      pipe_elements = c(pipe_elements, paste0("dplyr::mutate(`", var_to_replace, "` = ", calc_expr, ")"))
      pipe_elements = c(pipe_elements, "dplyr::ungroup()")
  } else {
      pipe_elements = c(pipe_elements, paste0("dplyr::mutate(`", var_to_replace, "` = ", calc_expr, ")"))
  }

  r_code_lines = c(paste0("data = ", paste(pipe_elements, collapse = " %>% \n  ")))

  return(paste(r_code_lines, collapse="\n"))
}



