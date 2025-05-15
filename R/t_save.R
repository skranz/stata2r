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

  has_replace = !is.na(options_part) && stringi::stri_detect_fixed(options_part, "replace")

  filename_r = "" # Resulting R path string or variable name

  if (is.na(filename_part) || filename_part == "") {
    # `save` without filename saves to the dataset currently in memory if it was read from disk,
    # or if a filename was specified in a previous `save` command.
    # This requires tracking dataset's origin filename. Complex.
    return("# `save` without filename not fully supported yet. Needs to track original data filename.")
  }

  # Check if filename_part is a macro `macroname`
  if (stringi::stri_startswith_fixed(filename_part, "`") && stringi::stri_endswith_fixed(filename_part, "'")) {
    macro_name = stringi::stri_sub(filename_part, 2, -2)

    # Find definition of this macro (from tempfile or other source)
    # Use convention: R_tempfile_L<def_line>_<macro>_path
    path_r_var = NA_character_
    defining_line_for_macro = NA_integer_

    for (i in (line_num - 1):1) {
      if (cmd_df$stata_cmd[i] == "tempfile" && grepl(paste0("\\b",macro_name,"\\b"), cmd_df$rest_of_cmd[i])) {
        path_r_var = paste0("R_tempfile_L", cmd_df$line[i], "_", macro_name, "_path")
        defining_line_for_macro = cmd_df$line[i]
        break
      }
    }
    if (!is.na(path_r_var)) {
      filename_r = path_r_var
      # Also, if Stata `save "`t1'"` implies that `t1` now refers to this *data content* for later R ops
      # (like merge using `t1` as an R dataframe), we need to assign `data` to an R variable.
      # Convention: R_tempdata_L<defining_line_of_macro>_<macroname>
      # This is for cases where the temp "file" is actually an in-memory R object for performance.
      r_temp_data_var = paste0("R_tempdata_L", defining_line_for_macro, "_", macro_name)
      save_to_r_object = paste0(r_temp_data_var, " = data # Storing data for Stata macro '", macro_name, "'")
    } else {
      warning(paste0("Macro ",filename_part, " in 'save' command at line ",line_num, " not resolved from tempfile. Treating as string."))
      filename_r = filename_part # fallback: treat as string `macroname`
      save_to_r_object = ""
    }
  } else {
    # Actual filename string, e.g. "mydata.dta"
    filename_r = filename_part
    save_to_r_object = "" # No R object for direct filename save unless explicitly needed by later logic
  }

  # `replace` option in Stata allows overwriting. `haven::write_dta` overwrites by default.
  # R code generation:
  # If save_to_r_object is needed (e.g. for tempfile `t1` that might be merged as an R df)
  # `R_tempdata_... = data`
  # `haven::write_dta(data, R_tempfile_..._path)` (or actual filename)

  r_code = ""
  if (save_to_r_object != "") {
    r_code = paste0(r_code, save_to_r_object, "\n")
  }
  r_code = paste0(r_code, "haven::write_dta(data, path = ", filename_r)
  # Stata `save` has version options; `haven::write_dta` has `version` argument.
  # For now, ignore Stata version options.
  r_code = paste0(r_code, ")")
  if (has_replace) {
    r_code = paste0(r_code, " # 'replace' option was used.")
  }

  return(r_code)
}


