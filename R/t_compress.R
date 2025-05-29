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
      # Use dplyr::mutate(across()) to apply sfun_compress_col_type
      vars_r_vec_str = paste0('c("', paste(vars_to_compress, collapse = '", "'), '")')
      r_code_lines = c(r_code_lines, paste0("data = dplyr::mutate(data, dplyr::across(dplyr::all_of(", vars_r_vec_str, "), sfun_compress_col_type))"))
  } else {
      # Apply to all variables using dplyr::across(dplyr::everything(), .fns = sfun_compress_col_type)
      # Using .fns = sfun_compress_col_type explicitly for clarity, though it might be inferred.
      r_code_lines = c(r_code_lines, paste0("data = dplyr::mutate(data, dplyr::across(dplyr::everything(), .fns = sfun_compress_col_type))"))
  }

  return(paste(r_code_lines, collapse="\n"))
}

