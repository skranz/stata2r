# FILE: R/t_append.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_append = function(rest_of_cmd) {
  restore.point("s2r_p_append")
  parts = stringi::stri_match_first_regex(stringi::stri_trim_both(rest_of_cmd), "^\\s*using\\s+(\"[^\"]+\"|`[^']+'|[^,\\s]+)(?:,\\s*(.*))?$")
  res = list(
    filename = if (!is.na(parts[1,1])) parts[1,2] else NA_character_,
    gen_var = NA_character_,
    options = if (!is.na(parts[1,1])) parts[1,3] else NA_character_
  )

  if (!is.na(res$options)) {
    gen_match = stringi::stri_match_first_regex(res$options, "\\b(?:gen|generate)\\s*\\(([^)]+)\\)")
    if (!is.na(gen_match[1,1])) {
      res$gen_var = stringi::stri_trim_both(gen_match[1,2])
    }
  }
  return(res)
}

# 2. Code Generation Phase: Emit R code
t_append = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_append")
  parsed = s2r_p_append(rest_of_cmd)

  if (is.na(parsed$filename)) {
    return(paste0("# Failed to parse append command: ", rest_of_cmd))
  }

  file_r_expr = resolve_stata_filename(parsed$filename, cmd_df, line_num, default_base_dir_var = "working_dir")

  args = c("data = data", paste0("file_path = ", file_r_expr))
  if (!is.na(parsed$gen_var)) {
    args = c(args, paste0("gen_var = ", quote_for_r_literal(parsed$gen_var)))
  }

  r_code = paste0("data = scmd_append(", paste(args, collapse = ", "), ")")

  r_code = paste0(r_code, "\nif (isTRUE(stata2r_env$has_original_order_idx)) { data = dplyr::mutate(data, stata2r_original_order_idx = dplyr::row_number()) }")

  return(r_code)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_append = function(data, file_path, gen_var = NA_character_) {
  restore.point("scmd_append")
  using_data = haven::read_dta(file_path)
  using_data = sfun_strip_stata_attributes(using_data)
  using_data = sfun_normalize_string_nas(using_data)

  data = sfun_normalize_string_nas(data)

  if (!is.na(gen_var)) {
    data[[gen_var]] = 0L
    using_data[[gen_var]] = 1L
  }

  data = dplyr::bind_rows(data, using_data)
  data = sfun_normalize_string_nas(data)

  return(data)
}
