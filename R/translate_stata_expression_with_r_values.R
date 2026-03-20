translate_stata_expression_with_r_values = function(stata_expr, line_num, cmd_df, context) {
  restore.point("translate_stata_expression_with_r_values")

  r_value_mappings = list()

  # --- Handle r() values from summarize/tabulate ---
  # Find the most recent summarize/tabulate command before the current line that was translated
  most_recent_r_producer_line_idx = NA_integer_
  for (i in rev(seq_len(line_num - 1L))) {
    if (cmd_df$stata_cmd[i] %in% c("summarize", "su", "tabulate", "tab") && cmd_df$do_translate[i]) {
      most_recent_r_producer_line_idx = i
      break
    }
  }

  if (!is.na(most_recent_r_producer_line_idx)) {
    prev_cmd_obj = cmd_df[most_recent_r_producer_line_idx, ]
    line_prefix_r = paste0("stata_r_val_L", prev_cmd_obj$line, "_")

    # Populate r_value_mappings based on stored runtime values in stata2r_env
    r_value_mappings[["r(N)"]] = paste0("stata2r_env$", line_prefix_r, "N")
    r_value_mappings[["r(mean)"]] = paste0("stata2r_env$", line_prefix_r, "mean")
    r_value_mappings[["r(sd)"]] = paste0("stata2r_env$", line_prefix_r, "sd")
    r_value_mappings[["r(min)"]] = paste0("stata2r_env$", line_prefix_r, "min")
    r_value_mappings[["r(max)"]] = paste0("stata2r_env$", line_prefix_r, "max")
    r_value_mappings[["r(sum)"]] = paste0("stata2r_env$", line_prefix_r, "sum")
    r_value_mappings[["r(p50)"]] = paste0("stata2r_env$", line_prefix_r, "p50")
  }

  # --- Handle e() values from estimation commands ---
  most_recent_e_producer_line_idx = NA_integer_
  for (i in rev(seq_len(line_num - 1L))) {
    if (cmd_df$stata_cmd[i] %in% stata_estimation_cmds && cmd_df$do_translate[i]) {
      most_recent_e_producer_line_idx = i
      break
    }
  }

  if (!is.na(most_recent_e_producer_line_idx)) {
    prev_cmd_obj_e = cmd_df[most_recent_e_producer_line_idx, ]
    line_prefix_e_base = paste0("stata_e_L", prev_cmd_obj_e$line, "_")

    if ("e(sample)" %in% unlist(prev_cmd_obj_e$e_results_needed)) {
      r_value_mappings[["e(sample)"]] = paste0("stata2r_env$stata_e_sample_L", prev_cmd_obj_e$line)
    }
    if ("e(N)" %in% unlist(prev_cmd_obj_e$e_results_needed)) {
      r_value_mappings[["e(N)"]] = paste0("stata2r_env$", line_prefix_e_base, "N")
    }
    if ("e(r2)" %in% unlist(prev_cmd_obj_e$e_results_needed)) {
      r_value_mappings[["e(r2)"]] = paste0("stata2r_env$", line_prefix_e_base, "r2")
    }
    if ("e(df_r)" %in% unlist(prev_cmd_obj_e$e_results_needed)) {
      r_value_mappings[["e(df_r)"]] = paste0("stata2r_env$", line_prefix_e_base, "df_r")
    }
    if ("e(rmse)" %in% unlist(prev_cmd_obj_e$e_results_needed)) {
      r_value_mappings[["e(rmse)"]] = paste0("stata2r_env$", line_prefix_e_base, "rmse")
    }
  }

  translated_expr = translate_stata_expression_to_r(
    stata_expr,
    context = context,
    r_value_mappings = r_value_mappings
  )
  return(translated_expr)
}
