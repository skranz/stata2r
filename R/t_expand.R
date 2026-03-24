# FILE: R/t_expand.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_expand = function(rest_of_cmd) {
  restore.point("s2r_p_expand")
  parsed = s2r_parse_if_in(rest_of_cmd)
  list(N_expr = parsed$base_str, if_str = parsed$if_str, in_str = parsed$in_str)
}

# 2. Code Generation Phase: Emit R code
t_expand = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_expand")
  parsed = s2r_p_expand(rest_of_cmd)
  if (is.na(parsed$N_expr) || parsed$N_expr == "") return(paste0("# expand requires N expression"))

  r_n_expr = translate_stata_expression_with_r_values(parsed$N_expr, line_num, cmd_df, list(is_by_group=FALSE))
  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, list(is_by_group=FALSE))
  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  args = c("data = data", paste0("r_n_expr_str = ", quote_for_r_literal(r_n_expr)))
  if (!is.na(r_if_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  if (!is.na(r_in_range)) args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))

  r_code = paste0("data = scmd_expand(", paste(args, collapse = ", "), ")")
  r_code = paste0(r_code, "\nif (isTRUE(stata2r_env$has_original_order_idx)) { data = dplyr::mutate(data, stata2r_original_order_idx = dplyr::row_number()) }")

  return(r_code)
}

# 3. Runtime Execution Phase: Evaluate against actual data
# 3. Runtime Execution Phase: Evaluate against actual data
scmd_expand = function(data, r_n_expr_str, r_if_cond = NA_character_, r_in_range = NA_character_) {
  restore.point("scmd_expand")
  mask = rep(TRUE, nrow(data))
  if (!is.na(r_if_cond) && r_if_cond != "") mask = mask & s2r_eval_cond(data, r_if_cond, envir = parent.frame())
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask = rep(FALSE, nrow(data))
    in_mask[idx] = TRUE
    mask = mask & in_mask
  }

  # Evaluate N expression inside data context properly
  eval_list = s2r_setup_eval_list(data)
  n_vals = base::eval(base::parse(text = r_n_expr_str), envir = eval_list, enclos = parent.frame())

  final_times = ifelse(mask, ifelse(is.na(n_vals), 1, pmax(0, as.integer(n_vals))), 1)

  data = data[rep(1:nrow(data), times = final_times), ]
  rownames(data) = NULL

  return(dplyr::as_tibble(data))
}
