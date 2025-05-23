# Translate Stata 'decode' command
# Stata: decode varname [if] [in] , gen(newvar) [options]
# Converts numeric varname with value labels into string newvar.

t_decode = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_decode") # Added restore.point
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
       return(paste0("# decode command requires varname: ", rest_of_cmd))
   }

  # Parse options, specifically `gen()`
  gen_var = NA_character_
  if (!is.na(options_str)) {
      gen_opt_match = stringi::stri_match_first_regex(options_str, "\\bgen\\s*\\(([^)]+)\\)")
      if (!is.na(gen_opt_match[1,1])) {
           gen_vars_str = stringi::stri_trim_both(gen_opt_match[1,2])
           gen_vars_list = stringi::stri_split_regex(gen_vars_str, "\\s+")[[1]]
           gen_var = gen_vars_list[1] # decode generates a single variable
       }
  }

  if (is.na(gen_var)) {
      return(paste0("# decode requires gen() option: ", rest_of_cmd))
  }

  # Translate if/in condition
  r_if_in_cond = NA_character_
  if (!is.na(stata_if_in_cond) && stata_if_in_cond != "") {
       r_if_in_cond = translate_stata_expression_with_r_values(stata_if_in_cond, line_num, cmd_df, context = list(is_by_group=FALSE))
       if (is.na(r_if_in_cond) || r_if_in_cond == "") {
           return(paste0("# Failed to translate if/in condition for decode: ", stata_if_in_cond))
       }
  }

  # R equivalent: as.character(labelled::to_factor(varname)) or similar
  # Stata decode uses value labels attached to the numeric variable (often created by encode).
  # haven package reads value labels into a "labelled" class. labelled::to_factor converts this.
  # as.character() converts the factor to strings.

  # Generate the new variable initialized to NA_character_ (Stata missing string is "")
  # Stata missing numeric decodes to missing string.
   r_code_lines = c(
      paste0("data = dplyr::mutate(data, ", gen_var, " = NA_character_)") # Initialize with NA string. Changed to dplyr::mutate
   )

  # Calculate decoded values
  # Use haven::as_factor or labelled::to_factor to respect value labels, then as.character.
  # haven::as_factor handles labelled class from read_dta.
  # Need to handle potential errors if the source variable isn't labelled correctly.
   r_code_lines = c(r_code_lines,
      paste0("## Decode values using haven::as_factor"),
      paste0("__decoded_values_L", cmd_obj$line, " = sfun_strip_stata_attributes(as.character(haven::as_factor(data$", varname_str, ", levels = 'labels')))") # 'labels' uses value labels
   )

  # Apply the if/in condition for replacement
  if (!is.na(r_if_in_cond) && r_if_in_cond != "") {
      # Replace values in gen_var where condition is true
       r_code_lines = c(r_code_lines,
           paste0("## Calculate condition flag"),
           paste0("__satisfies_cond_L", cmd_obj$line, " = ", r_if_in_cond),
           # Need to be careful with types in if_else. If gen_var is character, __decoded_values_L must be character.
           paste0("data = dplyr::mutate(data, ", gen_var, " = sfun_strip_stata_attributes(dplyr::if_else(__satisfies_cond_L", cmd_obj$line, ", __decoded_values_L", cmd_obj$line, ", ", gen_var, ")))"), # Changed to dplyr::mutate
           paste0("rm(__satisfies_cond_L", cmd_obj$line, ")")
       )
  } else {
      # Replace values in gen_var for all rows
      r_code_lines = c(r_code_lines,
           paste0("data = dplyr::mutate(data, ", gen_var, " = sfun_strip_stata_attributes(__decoded_values_L", cmd_obj$line, "))") # Changed to dplyr::mutate
      )
  }

  # Clean up temporary variable
  r_code_lines = c(r_code_lines, paste0("rm(__decoded_values_L", cmd_obj$line, ")"))


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

