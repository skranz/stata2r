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
      by_group_vars = character(0),
      by_sort_vars = character(0),
      is_quietly_prefix = logical(0),
      is_capture_prefix = logical(0), # NEW: Added capture prefix flag
      do_translate = logical(0),
      stata_translation_error = character(0),
      e_results_needed = I(vector("list", 0)), # New column for e() results
      r_results_needed = I(vector("list", 0)), # New column for r() results (future use)
      will_have_original_order_idx = logical(0), # Added by mark_data_manip_cmd logic
      will_ignore_row_order_for_comparison = logical(0), # NEW: Added for sort/gsort
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
      by_group_vars = if (length(parsed_info$by_group_vars)>0) paste(parsed_info$by_group_vars, collapse=",") else "",
      by_sort_vars = if (length(parsed_info$by_sort_vars)>0) paste(parsed_info$by_sort_vars, collapse=",") else "",
      is_quietly_prefix = parsed_info$is_quietly_prefix,
      is_capture_prefix = parsed_info$is_capture_prefix, # NEW: Assign capture prefix flag
      stata_translation_error = NA_character_,
      will_ignore_row_order_for_comparison = FALSE, # NEW: Initialize here
      stringsAsFactors = FALSE
    )
  })

  cmd_df = dplyr::bind_rows(cmd_list)
  # Initialize e_results_needed and r_results_needed as list columns
  cmd_df$e_results_needed = I(replicate(nrow(cmd_df), character(0), simplify = FALSE))
  cmd_df$r_results_needed = I(replicate(nrow(cmd_df), character(0), simplify = FALSE))
  # NEW: Initialize will_have_original_order_idx here too, it's safer
  cmd_df$will_have_original_order_idx = rep(FALSE, NROW(cmd_df)) 
  return(cmd_df)
}

