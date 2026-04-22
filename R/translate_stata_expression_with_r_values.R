translate_stata_expression_with_r_values = function(stata_expr, line_num, cmd_df, context) {
  restore.point("translate_stata_expression_with_r_values")

  r_value_mappings = list()

  if (line_num > 1L) {
    prev_cmds = cmd_df$stata_cmd[1:(line_num - 1L)]
    prev_trans = cmd_df$do_translate[1:(line_num - 1L)]

    # Handle r() values from the most recent translated r-class producer
    r_producers = c("summarize", "su", "tabulate", "tab")
    r_matches = which(prev_cmds %in% r_producers & prev_trans)

    if (length(r_matches) > 0) {
      prev_cmd_obj = cmd_df[r_matches[length(r_matches)], , drop = FALSE]
      needed_r = unlist(prev_cmd_obj$r_results_needed)

      if ("r(N)" %in% needed_r) {
        r_value_mappings[["r(N)"]] = "stata2r_env$stata_r_N"
      }
      if ("r(mean)" %in% needed_r) {
        r_value_mappings[["r(mean)"]] = "stata2r_env$stata_r_mean"
      }
      if ("r(sd)" %in% needed_r) {
        r_value_mappings[["r(sd)"]] = "stata2r_env$stata_r_sd"
      }
      if ("r(min)" %in% needed_r) {
        r_value_mappings[["r(min)"]] = "stata2r_env$stata_r_min"
      }
      if ("r(max)" %in% needed_r) {
        r_value_mappings[["r(max)"]] = "stata2r_env$stata_r_max"
      }
      if ("r(sum)" %in% needed_r) {
        r_value_mappings[["r(sum)"]] = "stata2r_env$stata_r_sum"
      }
      if ("r(p50)" %in% needed_r) {
        r_value_mappings[["r(p50)"]] = "stata2r_env$stata_r_p50"
      }
    }

    # Handle e() values from the most recent translated estimation producer
    e_matches = which(prev_cmds %in% stata_estimation_cmds & prev_trans)

    if (length(e_matches) > 0) {
      prev_cmd_obj_e = cmd_df[e_matches[length(e_matches)], , drop = FALSE]
      needed_e = unlist(prev_cmd_obj_e$e_results_needed)

      if ("e(sample)" %in% needed_e) {
        r_value_mappings[["e(sample)"]] = "stata2r_env$stata_e_sample"
      }
      if ("e(N)" %in% needed_e) {
        r_value_mappings[["e(N)"]] = "stata2r_env$stata_e_N"
      }
      if ("e(r2)" %in% needed_e) {
        r_value_mappings[["e(r2)"]] = "stata2r_env$stata_e_r2"
      }
      if ("e(df_r)" %in% needed_e) {
        r_value_mappings[["e(df_r)"]] = "stata2r_env$stata_e_df_r"
      }
      if ("e(rmse)" %in% needed_e) {
        r_value_mappings[["e(rmse)"]] = "stata2r_env$stata_e_rmse"
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

