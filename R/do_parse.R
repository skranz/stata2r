do_parse = function(do_code) {
  # do_code is a list of character vectors, each vector is a line
  # Ensure do_code is a simple character vector
  if (is.list(do_code)) {
      do_code = unlist(do_code)
  }
  # Ensure it's a character vector, even if empty after unlist
  if (!is.character(do_code)) {
    do_code = as.character(do_code)
  }


  num_lines = length(do_code)
  if (num_lines == 0) {
    return(data.frame(
      line = integer(0),
      do_code = character(0),
      stata_cmd_original = character(0),
      stata_cmd = character(0),
      rest_of_cmd = character(0),
      is_by_prefix = logical(0),
      by_group_vars = character(0), # Store as comma-separated string or list column
      by_sort_vars = character(0),  # Store as comma-separated string or list column
      do_translate = logical(0), # Added for consistency
      stata_translation_error = character(0), # New column
      stringsAsFactors = FALSE
    ))
  }

  cmd_list = lapply(seq_along(do_code), function(i) {
    line_text = do_code[i]
    parsed_info = parse_stata_command_line(line_text)
    data.frame(
      line = i,
      do_code = line_text,
      stata_cmd_original = parsed_info$stata_cmd_original,
      stata_cmd = parsed_info$stata_cmd,
      rest_of_cmd = parsed_info$rest_of_cmd,
      is_by_prefix = parsed_info$is_by_prefix,
      # Store by_group_vars and by_sort_vars as comma-separated strings
      by_group_vars = if (length(parsed_info$by_group_vars)>0) paste(parsed_info$by_group_vars, collapse=",") else NA_character_,
      by_sort_vars = if (length(parsed_info$by_sort_vars)>0) paste(parsed_info$by_sort_vars, collapse=",") else NA_character_,
      stata_translation_error = NA_character_, # Initialize as NA
      stringsAsFactors = FALSE
    )
  })

  cmd_df = dplyr::bind_rows(cmd_list)
  return(cmd_df)
}


