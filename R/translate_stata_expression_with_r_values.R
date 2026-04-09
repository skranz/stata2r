translate_stata_expression_with_r_values = function(stata_expr, line_num, cmd_df, context) {
  restore.point("translate_stata_expression_with_r_values")

  r_value_mappings = list()

  if (line_num > 1L) {
    # Extract vectors once (lightning fast) to avoid data.frame subsetting in loops
    prev_cmds = cmd_df$stata_cmd[1:(line_num - 1L)]
    prev_trans = cmd_df$do_translate[1:(line_num - 1L)]

    # --- Handle r() values from summarize/tabulate ---
    r_producers = c("summarize", "su", "tabulate", "tab")
    r_matches = which(prev_cmds %in% r_producers & prev_trans)

    if (length(r_matches) > 0) {
      most_recent_r_producer_line_idx = r_matches[length(r_matches)]
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
    e_matches = which(prev_cmds %in% stata_estimation_cmds & prev_trans)

    if (length(e_matches) > 0) {
      most_recent_e_producer_line_idx = e_matches[length(e_matches)]
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
  }

  translated_expr = translate_stata_expression_to_r(
    stata_expr,
    context = context,
    r_value_mappings = r_value_mappings
  )
  return(translated_expr)
}
