# Translate Stata 'rename' command
# Stata: rename old_var new_var
t_rename = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_rename") # Added restore.point
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # Split into old_var and new_var
  parts = stringi::stri_split_regex(rest_of_cmd_trimmed, "\\s+", n = 2)[[1]]

  if (length(parts) != 2) {
    return(paste0("# Failed to parse rename command: ", rest_of_cmd))
  }

  old_var = parts[1]
  new_var = parts[2]

  # Using collapse::frename
  # collapse::frename(data, old_name = new_name)
  r_code_str = paste0("data = collapse::frename(data, `", old_var, "` = `", new_var, "`)")
  # Alternative dplyr: data = dplyr::rename(data, new_var = old_var)
  # r_code_str = paste0("data = dplyr::rename(data, `", new_var, "` = `", old_var, "`)")

  return(r_code_str)
}

