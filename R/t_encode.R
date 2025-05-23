# Translate Stata 'encode' command
# Stata: encode varname [if] [in] , gen(newvar) [options]
# Converts string varname into numeric newvar with value labels.

t_encode = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_encode") # Added restore.point
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # Parse varname, if/in, options (especially gen())
  # Pattern: `varname [if] [in] , options`

  varname_str = NA_character_
  stata_if_in_cond = NA_character_
  options_str = NA_character_

  # Find if/in first
  if_in_match = stringi::stri_match_first_regex(rest_of_cmd_trimmed, "\\s+(?:if\\s+|in\\s+)(.*)$")
  if(!is.na(if_in_match[1,1])) {
      stata_if_in_cond = if_in_match[1,2]
      rest_no_if_in = stringi::stri_replace_last_regex(rest_of_cmd_trimmed, "\\s+(?:if\\s+|in\\s+)(.*)$", "")
      rest_no_if_in = stringi::stri_trim_both(rest_no_if_in)
  } else {
      rest_no_if_in = rest_of_cmd_trimmed
  }

   # Find options
  options_match = stringi::stri_match_first_regex(rest_no_if_in, ",\\s*(.*)$")
  if (!is.na(options_match[1,1])) {
      options_str = stringi::stri_trim_both(options_match[1,2])
      varname_str = stringi::stri_replace_last_regex(rest_no_if_in, ",\\s*(.*)$", "")
      varname_str = stringi::stri_trim_both(varname_str)
  } else {
      varname_str = rest_no_if_in
  }

   if (is.na(varname_str) || varname_str == "") {
       return(paste0("# encode command requires varname: ", rest_of_cmd))
   }

  # Parse options, specifically `gen()`
  gen_var = NA_character_
  if (!is.na(options_str)) {
      gen_opt_match = stringi::stri_match_first_regex(options_str, "\\bgen\\s*\\(([^)]+)\\)")
      if (!is.na(gen_opt_match[1,1])) {
           gen_vars_str = stringi::stri_trim_both(gen_opt_match[1,2])
           gen_vars_list = stringi::stri_split_regex(gen_vars_str, "\\s+")[[1]]
           gen_var = gen_vars_list[1] # encode generates a single variable
       }
  }

  if (is.na(gen_var)) {
      return(paste0("# encode requires gen() option: ", rest_of_cmd))
  }

  # Translate if/in condition
  r_if_in_cond = NA_character_
  if (!is.na(stata_if_in_cond) && stata_if_in_cond != "") {
       r_if_in_cond = translate_stata_expression_with_r_values(stata_if_in_cond, line_num, cmd_df, context = list(is_by_group=FALSE))
       if (is.na(r_if_in_cond) || r_if_in_cond == "") {
           return(paste0("# Failed to translate if/in condition for encode: ", stata_if_in_cond))
       }
  }

  # R equivalent: factor() or as.integer(factor())
  # Stata encode assigns integer codes (1, 2, ...) based on sorted unique values of the string variable.
  # R factor() does this by default. as.integer() retrieves the integer codes.
  # Value labels in R factor() correspond to Stata's encoded values.
  # Stata encode applies the gen option only to the variables listed before the comma.
  # If if/in is used, Stata encodes values *only for rows meeting the condition*.
  # For rows not meeting the condition, the new variable is set to missing.
  # This is similar to `gen newvar = .` followed by `replace newvar = ... if condition`.

   r_code_lines = c(
      # Generate the new variable initialized to NA (Stata missing)
      # Using dplyr::mutate
      paste0("data = dplyr::mutate(data, ", gen_var, " = NA_integer_)"), # Changed to dplyr::mutate
      # Apply encode logic only to rows meeting the condition (or all rows if no condition)
      # Need to select relevant rows and the variable, calculate encoded values, then replace.

      # Calculate encoded values: factor() on the source variable
      paste0("__encoded_values_L", cmd_obj$line, " = as.integer(base::factor(data$", varname_str, ", levels = base::unique(data$", varname_str, "[base::order(data$", varname_str, ")])))")
      # Note: base::factor default levels are sorted unique values, matching Stata behavior.

   )

  # Apply the if/in condition for replacement
  if (!is.na(r_if_in_cond) && r_if_in_cond != "") {
      # Replace values in gen_var where condition is true
       r_code_lines = c(r_code_lines,
           paste0("## Calculate condition flag"),
           paste0("__satisfies_cond_L", cmd_obj$line, " = ", r_if_in_cond),
           paste0("data = dplyr::mutate(data, ", gen_var, " = dplyr::if_else(__satisfies_cond_L", cmd_obj$line, ", __encoded_values_L", cmd_obj$line, ", ", gen_var, "))"), # Changed to dplyr::mutate
           paste0("rm(__satisfies_cond_L", cmd_obj$line, ")")
       )
  } else {
      # Replace values in gen_var for all rows
      r_code_lines = c(r_code_lines,
           paste0("data = dplyr::mutate(data, ", gen_var, " = __encoded_values_L", cmd_obj$line, ")") # Changed to dplyr::mutate
      )
  }

  # Clean up temporary variable
  r_code_lines = c(r_code_lines, paste0("rm(__encoded_values_L", cmd_obj$line, ")"))

  # Add value labels to the new factor column for completeness/diagnostics if using haven/labelled
  # This requires retrieving the original labels/levels.
  # R factor stores levels. haven::labelled can store value labels.
  # For now, just perform the numeric conversion.

  r_code_str = paste(r_code_lines, collapse="\n")

  # Add comment about options if any were present but not handled (excluding gen)
   options_str_cleaned = options_str
   if (!is.na(options_str_cleaned)) {
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "\\bgen\\s*\\([^)]+\\)", "")
        options_str_cleaned = stringi::stri_trim_both(stringi::stri_replace_all_regex(options_str_cleaned, ",+", ",")) # Clean up multiple commas
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "^,+", "") # Remove leading comma
   }

   if (!is.na(options_str_cleaned) && options_str_cleaned != "") {
        r_code_str = paste0(r_code_str, paste0(" # Other options ignored: ", options_str_cleaned))
   }


  return(r_code_str)
}

