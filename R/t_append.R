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

  raw_filename_token = stringi::stri_trim_both(append_match[1,2]) # Updated to raw_filename_token
  options_str = stringi::stri_trim_both(append_match[1,3]) # NA if no options

  # Resolve the `using filename` - can be a path string or a macro
  using_source_r = NA_character_ # This will hold the R expression to load the data

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

    path_r_var = NA_character_
    if (!is.na(found_def_line)) {
        path_r_var = paste0("R_tempfile_L", found_def_line, "_", macro_name, "_path")
    }

    if (!is.na(path_r_var)) {
        using_source_r = paste0("haven::read_dta(", path_r_var, ")")
    } else {
         warning(paste0("Macro ",raw_filename_token, " in 'append' command at line ",line_num, " not fully resolved. Treating as filename string."))
         using_source_r = paste0("haven::read_dta(", quote_for_r_literal(unquoted_content), ")")
    }
  } else {
    # Actual filename string, e.g. "mydata.dta" or mydata.dta (potentially unquoted in Stata)
    using_source_r = paste0("haven::read_dta(", quote_for_r_literal(unquoted_content), ")")
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

