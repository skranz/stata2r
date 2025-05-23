# Translate Stata 'save' command
# Stata: save [filename] [, options]
# Options: replace, emptyok, old (version control)
t_save = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  # Parse filename and options
  # Example: save "mydata.dta", replace
  # Example: save `tempfile_macro`, replace

  parts = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*([^,]*?)(?:,\\s*(.*))?$")
  raw_filename_token = stringi::stri_trim_both(parts[1,2]) # Can be empty (uses last used filename)
  options_part = stringi::stri_trim_both(parts[1,3])  # NA if no options

  filename_r = "" # Resulting R path string or variable name
  save_to_r_object = "" # R code to save data to an R object for direct access

  if (is.na(raw_filename_token) || raw_filename_token == "") {
    return("# `save` without filename not fully supported yet. Needs to track original data filename.")
  }

  # Extract the unquoted content for macro resolution or literal quoting
  unquoted_content = unquote_stata_string_or_macro_literal(raw_filename_token)

  # Check if filename_part is a macro `macroname`
  if (stringi::stri_startswith_fixed(raw_filename_token, "`") && stringi::stri_endswith_fixed(raw_filename_token, "'")) {
    macro_name = unquoted_content # Macro name is the unquoted content

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
      filename_r = paste0("R_tempfile_L", found_def_line, "_", macro_name, "_path")
      # Storing data to an R object for direct R access if tempfile macro was used.
      # This allows `use` or `merge using` commands to refer to the in-memory R object.
      save_to_r_object = paste0("R_tempdata_L", found_def_line, "_", macro_name, " = data # Storing data for Stata macro '", macro_name, "' for direct R access")
    } else {
      warning(paste0("Macro ",raw_filename_token, " in 'save' command at line ",line_num, " not resolved from tempfile. Treating as literal string."))
      filename_r = quote_for_r_literal(unquoted_content)
    }
  } else {
    # Actual filename string, e.g. "mydata.dta" or mydata.dta (potentially unquoted in Stata)
    filename_r = quote_for_r_literal(unquoted_content)
  }

  r_code = ""
  if (save_to_r_object != "") {
    r_code = paste0(r_code, save_to_r_object, "\n")
  }
  r_code = paste0(r_code, "haven::write_dta(data, path = ", filename_r, ")")
  
  if (!is.na(options_part) && options_part != "") {
    r_code = paste0(r_code, paste0(" # Options ignored: ", options_part))
  }

  return(r_code)
}

