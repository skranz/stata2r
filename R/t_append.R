# Translate Stata 'append' command
# Stata: append using filename [, options]

t_append = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_append") # Added restore.point
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # Parse `using filename` and options
  # Pattern: ^\s*using\s+([^,\s]+)(?:,\\s*(.*))?$
  # G1: filename (can be quoted or macro), G2: options

  append_match = stringi::stri_match_first_regex(rest_of_cmd_trimmed, "^\\s*using\\s+(\"[^\"]+\"|`[^']+'|[^,\\s]+)(?:,\\s*(.*))?$")

  if (is.na(append_match[1,1])) {
    return(paste0("# Failed to parse append command: ", rest_of_cmd))
  }

  raw_filename_token = stringi::stri_trim_both(append_match[1,2])
  options_str = stringi::stri_trim_both(append_match[1,3]) # NA if no options

  # Resolve the `using filename` - can be a path string or a macro
  using_source_r_expr = resolve_stata_filename(raw_filename_token, cmd_df, line_num, default_base_dir_var = "working_dir")

  # Parse generate() option
  gen_var_name = NA_character_
  if (!is.na(options_str)) {
    gen_opt_match = stringi::stri_match_first_regex(options_str, "\\b(?:gen|generate)\\s*\\(([^)]+)\\)")
    if (!is.na(gen_opt_match[1,1])) {
      gen_var_name = stringi::stri_trim_both(gen_opt_match[1,2])
    }
  }

  r_code_lines = c()
  temp_using_data_var = paste0("stata_tmp_using_data_L", line_num)
  temp_master_data_var = paste0("stata_tmp_master_data_L", line_num) # New temp var for master data before binding

  # Read using data, strip attributes and normalize string NAs
  r_code_lines = c(r_code_lines, paste0(temp_using_data_var, " = haven::read_dta(", using_source_r_expr, ")"))
  r_code_lines = c(r_code_lines, paste0(temp_using_data_var, " = sfun_strip_stata_attributes(", temp_using_data_var, ")")) # Added
  r_code_lines = c(r_code_lines, paste0(temp_using_data_var, " = sfun_normalize_string_nas(", temp_using_data_var, ")"))

  # Prepare master data for append, including original order idx and source flag
  r_code_lines = c(r_code_lines, paste0(temp_master_data_var, " = data"))
  # Normalize string NAs in master data before append, if not already done
  r_code_lines = c(r_code_lines, paste0(temp_master_data_var, " = sfun_normalize_string_nas(", temp_master_data_var, ")"))


  # If generate() option is present, add source indicator to both datasets
  if (!is.na(gen_var_name)) {
      r_code_lines = c(r_code_lines, paste0(temp_master_data_var, " = dplyr::mutate(", temp_master_data_var, ", `", gen_var_name, "` = 0L)"))
      r_code_lines = c(r_code_lines, paste0(temp_using_data_var, " = dplyr::mutate(", temp_using_data_var, ", `", gen_var_name, "` = 1L)"))
  }

  # Perform bind_rows
  r_code_lines = c(r_code_lines, paste0("data = dplyr::bind_rows(", temp_master_data_var, ", ", temp_using_data_var, ")"))
  # Ensure NAs in newly created character columns are converted to "" after bind_rows
  r_code_lines = c(r_code_lines, paste0("data = sfun_normalize_string_nas(data)"))

  # Clean up temporary variables
  r_code_lines = c(r_code_lines, paste0("rm(", temp_using_data_var, ", ", temp_master_data_var, ")"))

  r_code_str = paste(r_code_lines, collapse="\n")

  # Add comment about options if any were present but not handled
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


