do_parse = function(do_code) {
  # do_code is a list of character vectors, each vector is a line
  # Ensure do_code is a simple character vector
  if (is.list(do_code) && length(do_code) == 1 && is.character(do_code[[1]])){
      do_code = do_code[[1]]
  } else if (is.list(do_code)) {
      # If multiple elements in list, try to unlist if structure is simple
      # This might happen if stri_split_fixed returns a list of single strings
      do_code = unlist(do_code)
  }


  num_lines = length(do_code)
  if (num_lines == 0) {
    return(data.frame(
      line = integer(0),
      do_code = character(0),
      stata_cmd_original = character(0),
      stata_cmd = character(0),
      rest_of_cmd = character(0),
      by_vars = character(0),
      is_by_prefix = logical(0)
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
      by_vars = ifelse(is.na(parsed_info$by_vars), NA_character_, parsed_info$by_vars),
      is_by_prefix = parsed_info$is_by_prefix,
      stringsAsFactors = FALSE
    )
  })

  cmd_df = dplyr::bind_rows(cmd_list)
  return(cmd_df)
}

