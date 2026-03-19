# FILE: R/t_compress.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_compress = function(rest_of_cmd) {
  restore.point("s2r_p_compress")
  list(varlist = stringi::stri_trim_both(rest_of_cmd))
}

# 2. Code Generation Phase: Emit R code
t_compress = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_compress")
  parsed = s2r_p_compress(rest_of_cmd)

  args = c("data = data")
  if (!is.na(parsed$varlist) && parsed$varlist != "") {
    args = c(args, paste0("varlist_str = ", quote_for_r_literal(parsed$varlist)))
  }

  r_code_str = paste0("data = scmd_compress(", paste(args, collapse = ", "), ")")
  return(r_code_str)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_compress = function(data, varlist_str = NA_character_) {
  restore.point("scmd_compress")

  cols_to_compress = names(data)
  if (!is.na(varlist_str) && varlist_str != "") {
    # Dynamically expand variable patterns
    cols_to_compress = expand_varlist(varlist_str, names(data))
  }

  # Apply R implementation for Stata's compress logic
  for (col in cols_to_compress) {
    data[[col]] = sfun_compress_col_type(data[[col]])
  }

  return(data)
}
