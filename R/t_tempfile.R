# Translate Stata 'tempfile' command
# Stata: tempfile macroname1 [macroname2 ...]
t_tempfile = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_tempfile") # Added restore.point
  macro_names = stringi::stri_split_regex(stringi::stri_trim_both(rest_of_cmd), "\\s+")[[1]]
  macro_names = macro_names[macro_names != ""]

  if (length(macro_names) == 0) {
    return("# tempfile command with no macro names specified.")
  }

  r_code_lines = c()
  for (macro_name in macro_names) {
    # Stata `tempfile t1` creates a temporary filename and stores it in macro `t1`.
    # R equivalent: `temp_file_path = tempfile()`
    # We need to store this path in an R variable that subsequent commands (`save`, `use`, `merge`) can find
    # Convention: R_tempfile_L<line_num>_<macroname>_path
    # If the tempfile is used to store an R dataframe object (not just path), then R_tempdata_L...

    r_var_path = paste0("R_tempfile_L", line_num, "_", macro_name, "_path")
    # The R code generated will create this variable in its execution environment.
    r_code_lines = c(r_code_lines, paste0(r_var_path, " = tempfile(fileext = '.dta') # Stata tempfile '", macro_name, "'"))
  }

  return(paste(r_code_lines, collapse="\n"))
}


