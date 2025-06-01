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
      # This case implies `decode varname` without `, gen(newvar)` which is invalid for decode.
      # Stata syntax is `decode varname, gen(newvar)`. The comma is mandatory before options.
      # So, if no comma, there are no options, and thus no gen().
      # This means `gen_var` will be NA, and the error will be thrown later.
      return(paste0("# decode command requires gen() in options: ", rest_of_cmd))
  }

   if (is.na(varname_str) || varname_str == "") {
       return(paste0("# decode command requires varname: ", rest_of_cmd))
   }

  # Parse options, specifically `gen()`
  gen_var = NA_character_
  if (!is.na(options_str)) {
      # Modified regex to accept 'gen' or 'generate'
      gen_opt_match = stringi::stri_match_first_regex(options_str, "\\b(?:gen|generate)\\s*\\(([^)]+)\\)")
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
       # Context for if/in is global, not by_group specific for resolution, but _n/_N can be tricky
       r_if_in_cond = translate_stata_expression_with_r_values(stata_if_in_cond, line_num, cmd_df, context = list(is_by_group=FALSE))
       if (is.na(r_if_in_cond) || r_if_in_cond == "") {
           return(paste0("# Failed to translate if/in condition for decode: ", stata_if_in_cond))
       }
  }

  # Temporary variable names
  temp_decoded_values_L = paste0("stata_tmp_decoded_values_L", cmd_obj$line)
  temp_source_vector_L = paste0("stata_tmp_source_vector_L", cmd_obj$line)

   r_code_lines = c(
      paste0("data = dplyr::mutate(data, `", gen_var, "` = NA_character_)")
   )

   r_code_lines = c(r_code_lines,
      paste0("## Decode values using haven::as_factor"),
      paste0(temp_source_vector_L, " = data[['", varname_str, "']]"), # Store original values
      # Calculate decoded values. haven::as_factor converts unlabelled values to NA.
      # Corrected: removed `levels = 'labels'`
      paste0(temp_decoded_values_L, " = as.character(haven::as_factor(", temp_source_vector_L, "))"),
      # Stata's decode for unlabelled numeric values converts them to their string representation.
      # And missing values (NA) are converted to empty string "".
      paste0(temp_decoded_values_L, " = dplyr::if_else(is.na(", temp_decoded_values_L, "), ",
                                    "dplyr::if_else(is.na(", temp_source_vector_L, "), \"\", as.character(", temp_source_vector_L, ")), ",
                                    temp_decoded_values_L, ")")
   )

  # Apply the if/in condition for replacement
  if (!is.na(r_if_in_cond) && r_if_in_cond != "") {
       r_code_lines = c(r_code_lines,
           paste0("data = dplyr::mutate(data, `", gen_var, "` = dplyr::if_else((dplyr::coalesce(as.numeric(", r_if_in_cond, "), 0) != 0), ", temp_decoded_values_L, ", `", gen_var, "`))")
       )
  } else {
      r_code_lines = c(r_code_lines,
           paste0("data = dplyr::mutate(data, `", gen_var, "` = ", temp_decoded_values_L, ")")
      )
  }

  r_code_lines = c(r_code_lines, paste0("rm(", temp_decoded_values_L, ", ", temp_source_vector_L, ")"))

  r_code_str = paste(r_code_lines, collapse="\n")

   options_str_cleaned = options_str
   if (!is.na(options_str_cleaned)) {
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "\\b(?:gen|generate)\\s*\\([^)]+\\)", "")
        options_str_cleaned = stringi::stri_trim_both(stringi::stri_replace_all_regex(options_str_cleaned, ",+", ","))
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "^,+", "")
   }

   if (!is.na(options_str_cleaned) && options_str_cleaned != "") {
        r_code_str = paste0(r_code_str, paste0(" # Other options ignored: ", options_str_cleaned))
   }

  return(r_code_str)
}


