# Translate Stata 'format' command
# Stata: format varname %fmt
# This command only affects display, not the underlying data.

t_format = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_format")
  # Stata's `format` command only affects how data is displayed, not its underlying values or type.
  # It is now marked as `do_translate = FALSE` in `mark_data_manip_cmd.R`.
  # This function should ideally not be called if `do_translate` is FALSE.
  # Returning a comment as a fallback, though it should be skipped earlier.
  return(paste0("# Stata format command '", cmd_obj$stata_cmd_original, " ", rest_of_cmd, "' ignored for data transformation (already marked as non-data-manipulating)."))
}

