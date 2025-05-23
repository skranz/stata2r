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

  # Check if it's a Stata local macro: `macroname'
  if (stringi::stri_startswith_fixed(raw_filename_token, "`") && stringi::stri_endswith_fixed(raw_filename_token, "'")) {
    macro_name = stringi::stri_sub(raw_filename_token, 2, -2) # Extract macro name
    found_def_line = NA_integer_
    for (i in (line_num - 1):1) {
      if (cmd_df$stata_cmd[i] == "tempfile") {
        defined_macros = get_tempfile_macros(cmd_df$rest_of_cmd[i])
        if (macro_name %in% defined_macros) {
          found_def_line = cmd_df$line[i]
          break
        }
      }
    }
    
    if (!is.na(found_def_line)) {
      filename_r_expr = paste0("R_tempfile_L", found_def_line, "_", macro_name, "_path")
    } else {
      warning(paste0("Macro ",raw_filename_token, " in 'save' command at line ",line_num, " not resolved from tempfile. Treating as literal string."))
      filename_r_expr = quote_for_r_literal(unquote_stata_string_literal(raw_filename_token))
    }
  } else {
    unquoted_content = unquote_stata_string_literal(raw_filename_token)
    is_absolute_path = stringi::stri_startswith_fixed(unquoted_content, "/") || stringi::stri_detect_regex(unquoted_content, "^[A-Za-z]:[\\\\/]")
    if (is_absolute_path) {
      filename_r_expr = quote_for_r_literal(unquoted_content)
    } else {
      filename_r_expr = paste0("file.path(stata2r_env$working_dir, ", quote_for_r_literal(unquoted_content), ")")
    }
  }

  r_code = paste0("haven::write_dta(data, path = ", filename_r_expr, ")")
  
  if (!is.na(options_part) && options_part != "") {
    r_code = paste0(r_code, paste0(" # Options ignored: ", options_part))
  }

  return(r_code)
}


