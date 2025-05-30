translate_stata_expression_with_r_values = function(stata_expr, line_num, cmd_df, context) {
  restore.point("translate_stata_expression_with_r_values")

  r_value_mappings = list()

  # --- Handle r() values from summarize/tabulate ---
  # Find the most recent summarize/tabulate command before the current line that was translated
  most_recent_r_producer_line_idx = NA_integer_
  for (i in (line_num - 1):1) {
    if (cmd_df$stata_cmd[i] %in% c("summarize", "su", "tabulate", "tab") && cmd_df$do_translate[i]) {
      most_recent_r_producer_line_idx = i
      break
    }
  }

  if (!is.na(most_recent_r_producer_line_idx)) {
    prev_cmd_obj = cmd_df[most_recent_r_producer_line_idx,]
    # Assuming t_summarize or t_tabulate stores values like stata_r_val_L<line>_N, stata_r_val_L<line>_mean etc.
    line_prefix_r = paste0("stata_r_val_L", prev_cmd_obj$line, "_")

    # Populate r_value_mappings based on what t_summarize/t_tabulate might produce
    # This is a simplified list; a more robust solution would check prev_cmd_obj$r_results_needed
    # Common r() values from summarize:
    r_value_mappings[["r(N)"]] = paste0(line_prefix_r, "N")
    r_value_mappings[["r(mean)"]] = paste0(line_prefix_r, "mean")
    r_value_mappings[["r(sd)"]] = paste0(line_prefix_r, "sd")
    r_value_mappings[["r(min)"]] = paste0(line_prefix_r, "min")
    r_value_mappings[["r(max)"]] = paste0(line_prefix_r, "max")
    r_value_mappings[["r(sum)"]] = paste0(line_prefix_r, "sum")
    r_value_mappings[["r(p50)"]] = paste0(line_prefix_r, "p50") # Median
    # Add more as t_summarize implements them (e.g., p1, p5, Var, skewness, kurtosis)
  }

  # --- Handle e() values from estimation commands ---
  most_recent_e_producer_line_idx = NA_integer_
  for (i in (line_num - 1):1) {
    if (cmd_df$stata_cmd[i] %in% stata_estimation_cmds && cmd_df$do_translate[i]) {
      most_recent_e_producer_line_idx = i
      break
    }
  }

  if (!is.na(most_recent_e_producer_line_idx)) {
    prev_cmd_obj_e = cmd_df[most_recent_e_producer_line_idx,]
    line_prefix_e = paste0("stata_e_sample_L", prev_cmd_obj_e$line) # Specific to e(sample) for now

    if ("e(sample)" %in% unlist(prev_cmd_obj_e$e_results_needed)) {
      r_value_mappings[["e(sample)"]] = line_prefix_e
    }
    # Add mappings for other e() results like e(b), e(V) if t_regress etc. implement them
  }


  translated_expr = translate_stata_expression_to_r(stata_expr, context = context, r_value_mappings = r_value_mappings)
  return(translated_expr)
}


