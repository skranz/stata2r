# Translate Stata 'save' command
# Stata: save [filename] [, options]
# Options: replace, emptyok, old (version control)
t_save = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  # Parse filename and options
  # Example: save "mydata.dta", replace
  # Example: save `tempfile_macro`, replace

  parts = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*([^,]*?)(?:,\\s*(.*))?$")
  filename_part = stringi::stri_trim_both(parts[1,2]) # Can be empty (uses last used filename)
  options_part = stringi::stri_trim_both(parts[1,3])  # NA if no options

  filename_r = "" # Resulting R path string or variable name
  save_to_r_object = "" # R code to save data to an R object for direct access

  if (is.na(filename_part) || filename_part == "") {
    return("# `save` without filename not fully supported yet. Needs to track original data filename.")
  }

  # Check if filename_part is a macro `macroname`
  if (stringi::stri_startswith_fixed(filename_part, "`") && stringi::stri_endswith_fixed(filename_part, "'")) {
    macro_name = stringi::stri_sub(filename_part, 2, -2)

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
      warning(paste0("Macro ",filename_part, " in 'save' command at line ",line_num, " not resolved from tempfile. Treating as literal string."))
      filename_r = quote_for_r_literal(filename_part)
    }
  } else {
    # Actual filename string, e.g. "mydata.dta" or mydata.dta
    filename_r = quote_for_r_literal(filename_part)
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

