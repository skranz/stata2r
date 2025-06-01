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

  # Stata 'use filename' defaults to look in current working directory.
  # The 'working_dir' in stata2r_env represents this.
  # 'data_dir' is primarily for reference comparison files, or when explicitly specified via `using`.
  filename_r_expr = resolve_stata_filename(raw_filename_token, cmd_df, line_num, default_base_dir_var = "working_dir")

  r_code = paste0("data = haven::read_dta(", filename_r_expr, ")")

  # Strip haven attributes and normalize string NAs to empty strings ""
  r_code = paste0(r_code, " %>% \n  sfun_strip_stata_attributes() %>% \n  sfun_normalize_string_nas()")

  # Add a column to preserve the original row order, for use in `egen group()`/`tag()`
  r_code = paste0(r_code, " %>%\n  dplyr::mutate(stata2r_original_order_idx = dplyr::row_number())")
  # Set the global flag to TRUE, indicating that stata2r_original_order_idx is now present and should be maintained
  r_code = paste0(r_code, " %>% \n  { assign(\"has_original_order_idx\", TRUE, envir = stata2r_env); . }")


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


