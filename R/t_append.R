# Translate Stata 'append' command
# Stata: append using filename [, options]

t_append = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # Parse `using filename` and options
  # Pattern: ^\s*using\s+([^,\s]+)(?:,\\s*(.*))?$
  # G1: filename (can be quoted or macro), G2: options

  append_match = stringi::stri_match_first_regex(rest_of_cmd_trimmed, "^\\s*using\\s+(\"[^\"]+\"|`[^']+'|[^,\\s]+)(?:,\\s*(.*))?$")

  if (is.na(append_match[1,1])) {
    return(paste0("# Failed to parse append command: ", rest_of_cmd))
  }

  filename_part = stringi::stri_trim_both(append_match[1,2])
  options_str = stringi::stri_trim_both(append_match[1,3]) # NA if no options

    # Resolve the `using filename` - can be a path string or a macro
  using_source_r = NA_character_ # This will hold either a path string or an R variable name

  # Check if filename_part is a macro `macroname`
  if (stringi::stri_startswith_fixed(filename_part, "`") && stringi::stri_endswith_fixed(filename_part, "'")) {
    macro_name = stringi::stri_sub(filename_part, 2, -2)

    # Find definition of this macro (from tempfile or save)
    # Convention: R_tempfile_L<def_line>_<macro>_path OR R_tempdata_L<def_line>_<macro>
    path_r_var = NA_character_
    data_r_var = NA_character_

    # Scan cmd_df backwards for tempfile or save definition
    for (i in (line_num - 1):1) {
        if (cmd_df$stata_cmd[i] %in% c("tempfile", "save") && grepl(paste0("`",macro_name,"'"), cmd_df$rest_of_cmd[i])) {
            def_line = cmd_df$line[i]
            path_r_var = paste0("R_tempfile_L", def_line, "_", macro_name, "_path")
            data_r_var = paste0("R_tempdata_L", def_line, "_", macro_name)
            break
        }
    }

    # Prefer using the R data object if it exists, otherwise use the path
    if (!is.na(data_r_var)) {
        using_source_r = data_r_var
        # Need to ensure this variable name is treated as a dataframe in R code
        # e.g., `collapse::fbind(data, R_tempdata_LXX_macro)`
    } else if (!is.na(path_r_var)) {
        # Need to read the DTA file from this path
        # e.g., `collapse::fbind(data, haven::read_dta(R_tempfile_LXX_macro_path))`
        using_source_r = paste0("haven::read_dta(", path_r_var, ")")
    } else {
         # Fallback: assume macro holds a string path or is a string literal to read
         warning(paste0("Macro ",filename_part, " in 'append' command at line ",line_num, " not fully resolved. Treating as filename string."))
         using_source_r = filename_part
         using_source_r = paste0("haven::read_dta(", using_source_r, ")")
    }
  } else {
    # Actual filename string, e.g. "mydata.dta"
    using_source_r = filename_part
    using_source_r = paste0("haven::read_dta(", using_source_r, ")")
  }


  # Stata append requires variable names to match or be harmonized.
  # collapse::fbind matches columns by name. Differences are filled with NA. This is similar to Stata.
  # Options like `force` (append even if variable types don't match) are not handled.

  # Using collapse::fbind
  r_code_str = paste0("data = collapse::fbind(data, ", using_source_r, ")")

  # Add comment about options if any were present but not handled
  if (!is.na(options_str) && options_str != "") {
       r_code_str = paste0(r_code_str, paste0(" # Options ignored: ", options_str))
  }


  return(r_code_str)
}

