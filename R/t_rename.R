# FILE: R/t_rename.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_rename = function(rest_of_cmd) {
  restore.point("s2r_p_rename")
  parts = stringi::stri_split_regex(stringi::stri_trim_both(rest_of_cmd), "\\s+", n = 2)[[1]]
  list(
    old_var = if(length(parts) >= 1) parts[1] else NA_character_,
    new_var = if(length(parts) == 2) parts[2] else NA_character_
  )
}

# 2. Code Generation Phase: Emit R code
t_rename = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_rename")
  parsed = s2r_p_rename(rest_of_cmd)

  if (is.na(parsed$old_var) || is.na(parsed$new_var)) {
    return(paste0("# Failed to parse rename command: ", rest_of_cmd))
  }

  # Build call to runtime execution function `scmd_rename`
  args = c("data = data",
           paste0("old_var = ", quote_for_r_literal(parsed$old_var)),
           paste0("new_var = ", quote_for_r_literal(parsed$new_var)))

  r_code_str = paste0("data = scmd_rename(", paste(args, collapse = ", "), ")")
  return(r_code_str)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_rename = function(data, old_var, new_var) {
  restore.point("scmd_rename")

  # Allow runtime abbreviation for old_var
  expanded_old = expand_varlist(old_var, names(data))
  if (length(expanded_old) == 0) {
    stop(paste0("scmd_rename: variable '", old_var, "' not found"))
  }

  old_var_actual = expanded_old[1]

  # Base R implementation is fastest and robust here
  names(data)[names(data) == old_var_actual] = new_var

  return(data)
}
