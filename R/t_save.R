# FILE: R/t_save.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_save = function(rest_of_cmd) {
  restore.point("s2r_p_save")
  parts = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*([^,]*?)(?:,\\s*(.*))?$")
  list(
    raw_filename = stringi::stri_trim_both(parts[1,2]),
    options = stringi::stri_trim_both(parts[1,3])
  )
}

# 2. Code Generation Phase: Emit R code
t_save = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_save")

  return("# we don't translate stata save commands to R.")


  parsed = s2r_p_save(rest_of_cmd)

  if (is.na(parsed$raw_filename) || parsed$raw_filename == "") {
    return("# `save` without filename not fully supported yet. Needs to track original data filename.")
  }

  # resolve_stata_filename returns an R expression evaluating to the path dynamically
  filename_r_expr = resolve_stata_filename(parsed$raw_filename, cmd_df, line_num, default_base_dir_var = "working_dir")

  args = c("data = data",
           paste0("file_path = ", filename_r_expr))

  r_code_str = paste0("data = scmd_save(", paste(args, collapse = ", "), ")")

  if (!is.na(parsed$options) && parsed$options != "") {
    r_code_str = paste0(r_code_str, " # Options ignored: ", parsed$options)
  }

  return(r_code_str)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_save = function(data, file_path) {
  restore.point("scmd_save")
  haven::write_dta(data, path = file_path)

  # Return data unchanged
  return(data)
}
