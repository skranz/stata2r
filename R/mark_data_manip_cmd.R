mark_data_manip_cmd = function(cmd_df) {
  restore.point("mark_data_manip_cmd")

  if (NROW(cmd_df) == 0) {
    cmd_df = s2r_check_mod_df(cmd_df)
    if (!("will_have_original_order_idx" %in% names(cmd_df))) cmd_df$will_have_original_order_idx = logical(0)
    if (!("will_ignore_row_order_for_comparison" %in% names(cmd_df))) cmd_df$will_ignore_row_order_for_comparison = logical(0)
    return(cmd_df)
  }

  cmd_df = s2r_check_mod_df(cmd_df)

  if (!("will_have_original_order_idx" %in% names(cmd_df))) {
    cmd_df$will_have_original_order_idx = rep(FALSE, NROW(cmd_df))
  }
  cmd_df$will_ignore_row_order_for_comparison = rep(FALSE, NROW(cmd_df))

  # Determine if stata2r_original_order_idx will be present at this line's execution
  current_has_order_idx_at_translation_time = FALSE
  for (i in seq_len(NROW(cmd_df))) {
    if (cmd_df$do_translate[i]) {
      if (cmd_df$stata_cmd[i] %in% c("use", "collapse", "reshape")) {
        current_has_order_idx_at_translation_time = TRUE
      } else if (cmd_df$stata_cmd[i] %in% c("drop", "keep", "expand", "merge", "append", "order")) {
        current_has_order_idx_at_translation_time = current_has_order_idx_at_translation_time
      } else if (cmd_df$stata_cmd[i] %in% c("preserve")) {
        current_has_order_idx_at_translation_time = current_has_order_idx_at_translation_time
      } else if (cmd_df$stata_cmd[i] %in% c("restore")) {
        current_has_order_idx_at_translation_time = current_has_order_idx_at_translation_time
      }
    }
    cmd_df$will_have_original_order_idx[i] = current_has_order_idx_at_translation_time
  }

  # Ignore row order in comparison for sort and gsort
  cmd_df$will_ignore_row_order_for_comparison[cmd_df$stata_cmd %in% c("sort", "gsort")] = TRUE

  return(cmd_df)
}
