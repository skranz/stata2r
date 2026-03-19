# FILE: R/t_use.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_use = function(rest_of_cmd) {
  restore.point("s2r_p_use")
  parts = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*(\"[^\"]+\"|`[^']+'|[^,\\s]+)\\s*(?:,\\s*(clear))?")
  list(
    filename = if (!is.na(parts[1,1])) parts[1,2] else NA_character_,
    clear = !is.na(parts[1,3])
  )
}

# 2. Code Generation Phase: Emit R code
t_use = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_use")
  parsed = s2r_p_use(rest_of_cmd)

  if (is.na(parsed$filename)) {
    return(paste0("# Failed to parse use command: ", rest_of_cmd))
  }

  file_r_expr = resolve_stata_filename(parsed$filename, cmd_df, line_num, default_base_dir_var = "working_dir")

  args = c(paste0("file_path = ", file_r_expr), paste0("clear = ", parsed$clear))
  r_code = paste0("data = scmd_use(", paste(args, collapse = ", "), ")")

  # Set global index tracking
  r_code = paste0(r_code, "\nassign(\"has_original_order_idx\", TRUE, envir = stata2r_env)")

  return(r_code)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_use = function(file_path, clear = FALSE) {
  restore.point("scmd_use")
  data = haven::read_dta(file_path)
  data = sfun_strip_stata_attributes(data)
  data = sfun_normalize_string_nas(data)

  # Ensure the index is preserved correctly
  data$stata2r_original_order_idx = seq_len(nrow(data))

  return(data)
}
