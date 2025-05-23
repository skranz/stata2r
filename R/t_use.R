t_use = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_use")
  # Example: use "filename.dta", clear
  #          use "`macroname'", clear

  parts = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*(\"[^\"]+\"|`[^']+'|[^,\\s]+)\\s*(?:,\\s*(clear))?")
  # Group 1: filename (quoted or macro or unquoted literal)
  # Group 2: clear (optional)

  if (is.na(parts[1,1])) {
    return(paste0("# Failed to parse use command: ", rest_of_cmd))
  }

  raw_filename_token = parts[1,2]
  clear_opt = parts[1,3] # NA if not present, "clear" if present

  filename_r_expr = NA_character_ # This will hold the R path expression

  # Extract the unquoted content for macro resolution or literal quoting
  unquoted_content = unquote_stata_string_or_macro_literal(raw_filename_token)

  # Handle macro `filename'
  if (stringi::stri_startswith_fixed(raw_filename_token, "`") && stringi::stri_endswith_fixed(raw_filename_token, "'")) {
    macro_name = unquoted_content # Macro name is the unquoted content
    
    found_def_line = NA_integer_
    for (i in (line_num - 1):1) {
        if (cmd_df$stata_cmd[i] == "tempfile") {
            defined_macros = get_tempfile_macros(cmd_df$rest_of_cmd[i])
            if (macro_name %in% defined_macros) { # Check if macro name matches
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
        filename_r_expr = path_r_var # This is an R variable name, no quotes needed
    } else {
        warning(paste0("Macro ",raw_filename_token, " in 'use' command at line ",line_num, " may not be correctly resolved. Treating as literal string."))
        filename_r_expr = quote_for_r_literal(unquoted_content) # Treat as literal and add R quotes
    }
  } else {
    # Actual filename string, e.g. "mydata.dta" or mydata.dta (potentially unquoted in Stata)
    # Determine if it's an absolute path or relative, and prepend data_dir if relative.
    is_absolute_path = stringi::stri_startswith_fixed(unquoted_content, "/") || stringi::stri_detect_regex(unquoted_content, "^[A-Za-z]:[\\\\/]")
    if (is_absolute_path) {
      filename_r_expr = quote_for_r_literal(unquoted_content) # Use as is if absolute
    } else {
      # Assume relative path for 'use' refers to the data_dir
      filename_r_expr = paste0("file.path(stata2r_env$data_dir, ", quote_for_r_literal(unquoted_content), ")")
    }
  }

  r_code = paste0("data = haven::read_dta(path = ", filename_r_expr, ")")

  # `clear` option in Stata allows overwriting. R `read_dta` just overwrites.
  # So no special handling needed for `clear` in R code.
  # Using haven::read_dta
  # Assuming Stata .dta files. If other types, logic needs extension.

  # Add a comment about 'clear' if it was used
  if (!is.na(clear_opt)) {
    r_code = paste0(r_code, " # 'clear' was used")
  }

  return(r_code)
}

