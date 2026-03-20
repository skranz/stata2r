do_parse = function(do_code) {
  if (is.list(do_code)) {
    do_code = unlist(do_code)
  }
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
      is_bysort_prefix = logical(0),
      by_group_vars = character(0),
      by_sort_vars = character(0),
      is_quietly_prefix = logical(0),
      is_capture_prefix = logical(0),
      is_xi_prefix = logical(0),
      do_translate = logical(0),
      is_mod = logical(0),
      need_e_sample = logical(0),
      need_xi = logical(0),
      need_e_results = logical(0),
      need_r_results = logical(0),
      stata_translation_error = character(0),
      e_results_needed = I(vector("list", 0)),
      r_results_needed = I(vector("list", 0)),
      will_have_original_order_idx = logical(0),
      will_ignore_row_order_for_comparison = logical(0),
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
      is_bysort_prefix = parsed_info$is_bysort_prefix,
      by_group_vars = if (length(parsed_info$by_group_vars) > 0) paste(parsed_info$by_group_vars, collapse = ",") else "",
      by_sort_vars = if (length(parsed_info$by_sort_vars) > 0) paste(parsed_info$by_sort_vars, collapse = ",") else "",
      is_quietly_prefix = parsed_info$is_quietly_prefix,
      is_capture_prefix = parsed_info$is_capture_prefix,
      is_xi_prefix = parsed_info$is_xi_prefix,
      stata_translation_error = NA_character_,
      will_ignore_row_order_for_comparison = FALSE,
      stringsAsFactors = FALSE
    )
  })

  cmd_df = dplyr::bind_rows(cmd_list)

  cmd_df$e_results_needed = I(replicate(nrow(cmd_df), character(0), simplify = FALSE))
  cmd_df$r_results_needed = I(replicate(nrow(cmd_df), character(0), simplify = FALSE))
  cmd_df$will_have_original_order_idx = rep(FALSE, NROW(cmd_df))
  cmd_df$do_translate = rep(FALSE, NROW(cmd_df))
  cmd_df$is_mod = rep(FALSE, NROW(cmd_df))
  cmd_df$need_e_sample = rep(FALSE, NROW(cmd_df))
  cmd_df$need_xi = rep(FALSE, NROW(cmd_df))
  cmd_df$need_e_results = rep(FALSE, NROW(cmd_df))
  cmd_df$need_r_results = rep(FALSE, NROW(cmd_df))

  return(cmd_df)
}
