# Translate Stata 'encode' command
# Stata: encode varname [if] [in] , gen(newvar) [options]
# Converts string varname into numeric newvar with value labels.

t_encode = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_encode")
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # Parse varname, if/in, options (especially gen())
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

   # Find options (which contain gen())
  options_match = stringi::stri_match_first_regex(rest_no_if_in, ",\\s*(.*)$")
  if (!is.na(options_match[1,1])) {
      options_str = stringi::stri_trim_both(options_match[1,2])
      varname_str = stringi::stri_replace_last_regex(rest_no_if_in, ",\\s*(.*)$", "")
      varname_str = stringi::stri_trim_both(varname_str)
  } else {
      # This case implies `encode varname` without `, gen(newvar)` which is invalid for encode.
      # Stata syntax is `encode varname, gen(newvar)`. The comma is mandatory before options.
      # So, if no comma, there are no options, and thus no gen().
      # This means `gen_var` will be NA, and the error will be thrown later.
      return(paste0("# encode command requires gen() in options: ", rest_of_cmd))
  }

   if (is.na(varname_str) || varname_str == "") {
       return(paste0("# encode command requires varname: ", rest_of_cmd))
   }

  # Parse options, specifically `gen()` or `generate()`
  gen_var = NA_character_
  if (!is.na(options_str)) {
      # Modified regex to accept 'gen' or 'generate'
      gen_opt_match = stringi::stri_match_first_regex(options_str, "\\b(?:gen|generate)\\s*\\(([^)]+)\\)")
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

  # Temporary variable names for the fully calculated encoded vector and condition
  encoded_values_full_tmp_var = paste0("stata_tmp_encoded_full_L", cmd_obj$line)
  
  r_code_lines = c()

  # Initialize the new column as an integer vector. Labels will be applied by label values.
  r_code_lines = c(r_code_lines,
    paste0("data = dplyr::mutate(data, `", gen_var, "` = NA_integer_)")
  )

  # Calculate the full encoded vector (based on all unique values in source column, sorted alphabetically)
  # Stata encode assigns numeric codes (1, 2, ...) to non-missing string values, preserving missing values as NA.
  r_code_lines = c(r_code_lines,
    paste0("temp_source_vector_L", cmd_obj$line, " = data[['", varname_str, "']]"),
    # Get unique, non-NA, sorted string values to define the mapping to numeric codes
    paste0("temp_unique_values_L", cmd_obj$line, " = base::sort(base::unique(temp_source_vector_L", cmd_obj$line, "[!is.na(temp_source_vector_L", cmd_obj$line, ")]))"),
    # Create the integer codes (1, 2, ...) based on the sorted unique values. NA values in source remain NA.
    paste0("temp_numeric_values_L", cmd_obj$line, " = base::match(temp_source_vector_L", cmd_obj$line, ", temp_unique_values_L", cmd_obj$line, ")"),
    # Create the labels vector in the R code for haven::labelled
    paste0("temp_labels_vector_L", cmd_obj$line, " = stats::setNames(as.numeric(1:length(temp_unique_values_L", cmd_obj$line, ")), temp_unique_values_L", cmd_obj$line, ")"),
    # Create the haven::labelled object
    paste0(encoded_values_full_tmp_var, " = haven::labelled(as.integer(temp_numeric_values_L", cmd_obj$line, "), labels = temp_labels_vector_L", cmd_obj$line, ")"),
    # Clean up intermediate temp variables
    paste0("rm(temp_source_vector_L", cmd_obj$line, ", temp_unique_values_L", cmd_obj$line, ", temp_numeric_values_L", cmd_obj$line, ", temp_labels_vector_L", cmd_obj$line, ")")
  )

  # Apply the if/in condition for assignment to the target column in 'data'
  if (!is.na(r_if_in_cond) && r_if_in_cond != "") {
       r_code_lines = c(r_code_lines,
           paste0("data = dplyr::mutate(data, `", gen_var, "` = dplyr::if_else((dplyr::coalesce(as.numeric(", r_if_in_cond, "), 0) != 0), ", encoded_values_full_tmp_var, ", `", gen_var, "`))")
       )
  } else {
      # No condition, assign the full encoded vector
      r_code_lines = c(r_code_lines,
           paste0("data[['", gen_var, "']] = ", encoded_values_full_tmp_var)
      )
  }

  r_code_lines = c(r_code_lines, paste0("rm(", encoded_values_full_tmp_var, ")"))

  r_code_str = paste(r_code_lines, collapse="\n")

  # Add comment about other options if any were present but not handled (excluding gen)
   options_str_cleaned = options_str
   if (!is.na(options_str_cleaned)) {
        # Modify regex to remove both 'gen' and 'generate' forms
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "\\b(?:gen|generate)\\s*\\([^)]+\\)", "")
        options_str_cleaned = stringi::stri_trim_both(stringi::stri_replace_all_regex(options_str_cleaned, ",+", ","))
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "^,+", "") # Remove leading comma
   }

   if (!is.na(options_str_cleaned) && options_str_cleaned != "") {
        r_code_str = paste0(r_code_str, paste0("\n# Other options ignored: ", options_str_cleaned))
   }

  return(r_code_str)
}

