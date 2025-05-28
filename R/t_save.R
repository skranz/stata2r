# Translate Stata 'save' command
# Stata: save [filename] [, options]
# Options: replace, emptyok, old (version control)
t_save = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_save") # Added restore.point
  # Parse filename and options
  # Example: save "mydata.dta", replace
  # Example: save `tempfile_macro`, replace

  parts = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*([^,]*?)(?:,\\s*(.*))?$")
  raw_filename_token = stringi::stri_trim_both(parts[1,2]) # Can be empty (uses last used filename)
  options_part = stringi::stri_trim_both(parts[1,3])  # NA if no options

  filename_r_expr = "" # Resulting R path string or variable name

  if (is.na(raw_filename_token) || raw_filename_token == "") {
    return("# `save` without filename not fully supported yet. Needs to track original data filename.")
  }

  filename_r_expr = resolve_stata_filename(raw_filename_token, cmd_df, line_num, default_base_dir_var = "working_dir")

  r_code = paste0("haven::write_dta(data, path = ", filename_r_expr, ")")
  
  if (!is.na(options_part) && options_part != "") {
    r_code = paste0(r_code, paste0(" # Options ignored: ", options_part))
  }

  return(r_code)
}

