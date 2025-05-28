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
  # Changed default_base_dir_var to "working_dir" for consistency with Stata's default file paths
  using_source_r_expr = resolve_stata_filename(raw_filename_token, cmd_df, line_num, default_base_dir_var = "working_dir")

  # Stata append requires variable names to match or be harmonized.
  # dplyr::bind_rows matches columns by name. Differences are filled with NA. This is similar to Stata.
  # Options like `force` (append even if variable types don't match) are not handled.

  # Using dplyr::bind_rows instead of collapse::fbind (due to reported "not exported" error)
  r_code_str = paste0("data = dplyr::bind_rows(data, haven::read_dta(", using_source_r_expr, "))")

  # Add comment about options if any were present but not handled
  if (!is.na(options_str) && options_str != "") {
       r_code_str = paste0(r_code_str, paste0(" # Options ignored: ", options_str))
  }


  return(r_code_str)
}

