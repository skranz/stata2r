# FILE: R/t_tempfile.R

# 1. Parsing Phase
s2r_p_tempfile = function(rest_of_cmd) {
  names = stringi::stri_split_regex(stringi::stri_trim_both(rest_of_cmd), "\\s+")[[1]]
  list(macros = names[names != ""])
}

# 2. Code Generation Phase
t_tempfile = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_tempfile")
  parsed = s2r_p_tempfile(rest_of_cmd)

  if (length(parsed$macros) == 0) return("# tempfile command with no macro names specified.")

  r_code = c()
  for (m in parsed$macros) {
    r_code = c(r_code, paste0("R_tempfile_L", line_num, "_", m, "_path = tempfile(fileext = '.dta')"))
  }
  return(paste(r_code, collapse="\n"))
}
