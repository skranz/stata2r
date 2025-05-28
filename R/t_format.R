# Translate Stata 'format' command
# Stata: format varname %fmt
# This command only affects display, not the underlying data.

t_format = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_format")
  # Stata's `format` command only affects how data is displayed, not its underlying values or type.
  # For data equivalence translation, it can largely be ignored or noted.
  return(paste0("# Stata format command '", cmd_obj$stata_cmd_original, " ", rest_of_cmd, "' ignored for data transformation."))
}

