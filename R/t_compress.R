# Translate Stata 'compress' command
# Stata: compress [varlist]
# Changes storage type to smallest necessary.

t_compress = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_compress")
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # `compress` can take a varlist. If varlist is empty, it means all variables.
  vars_to_compress = character(0)
  if (rest_of_cmd_trimmed != "") {
      vars_to_compress = stringi::stri_split_regex(rest_of_cmd_trimmed, "\\s+")[[1]]
      vars_to_compress = vars_to_compress[vars_to_compress != ""]
  }

  r_code_lines = c()
  if (length(vars_to_compress) > 0) {
      # Use collapse::fcompress on specific columns
      vars_r_vec_str = paste0('c("', paste(vars_to_compress, collapse = '", "'), '")')
      r_code_lines = c(r_code_lines, paste0("data = collapse::fcompress(data, cols = ", vars_r_vec_str, ")"))
  } else {
      # Use collapse::fcompress on the entire dataframe
      r_code_lines = c(r_code_lines, paste0("data = collapse::fcompress(data)"))
  }

  return(paste(r_code_lines, collapse="\n"))
}

