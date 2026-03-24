# FILE: R/t_collapse.R

# 1. Parsing Phase
s2r_p_collapse = function(rest_of_cmd) {
  restore.point("s2r_p_collapse")
  parts = stringi::stri_match_first_regex(stringi::stri_trim_both(rest_of_cmd), "^\\s*(.*?)(?:,\\s*(.*))?$")
  agg_part = stringi::stri_trim_both(parts[1,2])
  options_part = stringi::stri_trim_both(parts[1,3])

  parsed = s2r_parse_if_in(agg_part)
  agg_part = parsed$base_str

  agg_matches = stringi::stri_match_all_regex(agg_part, "\\(([a-zA-Z_][a-zA-Z0-9_]*)\\)\\s*([a-zA-Z_][a-zA-Z0-9_.]*)(?:\\s*=\\s*([^,]+))?")[[1]]

  by_vars = character(0)
  if (!is.na(options_part)) {
    by_opt = stringi::stri_match_first_regex(options_part, "\\bby\\s*\\(([^)]+)\\)")
    if (!is.na(by_opt[1,1])) by_vars = stringi::stri_split_regex(stringi::stri_trim_both(by_opt[1,2]), "\\s+")[[1]]
  }

  list(aggs = agg_matches, by_vars = by_vars[by_vars != ""], if_str = parsed$if_str, in_str = parsed$in_str, raw_options = options_part)
}

# 2. Code Generation Phase
t_collapse = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_collapse")
  parsed = s2r_p_collapse(rest_of_cmd)
  if (NROW(parsed$aggs) == 0) return(paste0("# Failed to parse collapse aggregate definitions: ", rest_of_cmd))

  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, list(is_by_group = FALSE))

  agg_list = list()
  for (j in 1:NROW(parsed$aggs)) {
    stat = parsed$aggs[j, 2]
    target_var = stringi::stri_trim_both(parsed$aggs[j, 3])
    source_expr = stringi::stri_trim_both(parsed$aggs[j, 4])
    if (is.na(source_expr) || source_expr == "") source_expr = target_var

    r_source = translate_stata_expression_with_r_values(source_expr, line_num, cmd_df, context)
    if (is.na(r_source)) return(paste0("# Failed to translate source expr: ", source_expr))

    collapse_func = switch(stat,
      "mean" = paste0("collapse::fmean(", r_source, ", na.rm = TRUE)"),
      "sum" = paste0("collapse::fsum(", r_source, ", na.rm = TRUE)"),
      "count" = paste0("sum(!is.na(", r_source, "))"),
      "N" = "dplyr::n()",
      "first" = paste0("collapse::ffirst(", r_source, ")"),
      "last" = paste0("collapse::flast(", r_source, ")"),
      "min" = paste0("collapse::fmin(", r_source, ", na.rm = TRUE)"),
      "max" = paste0("collapse::fmax(", r_source, ", na.rm = TRUE)"),
      "median" = paste0("collapse::fmedian(", r_source, ", na.rm = TRUE)"),
      "sd" = paste0("collapse::fsd(", r_source, ", na.rm = TRUE)"),
      "p1" = paste0("collapse::fquantile(", r_source, ", probs = 0.01, na.rm = TRUE)"),
      "p5" = paste0("collapse::fquantile(", r_source, ", probs = 0.05, na.rm = TRUE)"),
      "p10" = paste0("collapse::fquantile(", r_source, ", probs = 0.10, na.rm = TRUE)"),
      "p25" = paste0("collapse::fquantile(", r_source, ", probs = 0.25, na.rm = TRUE)"),
      "p75" = paste0("collapse::fquantile(", r_source, ", probs = 0.75, na.rm = TRUE)"),
      "p90" = paste0("collapse::fquantile(", r_source, ", probs = 0.90, na.rm = TRUE)"),
      "p95" = paste0("collapse::fquantile(", r_source, ", probs = 0.95, na.rm = TRUE)"),
      "p99" = paste0("collapse::fquantile(", r_source, ", probs = 0.99, na.rm = TRUE)"),
      NULL
    )
    if (is.null(collapse_func)) return(paste0("# Collapse stat '", stat, "' not implemented."))
    agg_list[[target_var]] = collapse_func
  }

  agg_r_list_str = paste0("list(", paste(sprintf("`%s` = \"%s\"", names(agg_list), gsub('"', '\\\\"', unlist(agg_list))), collapse=", "), ")")

  args = c("data = data", paste0("agg_exprs_list = ", agg_r_list_str))
  if (length(parsed$by_vars) > 0) args = c(args, paste0("group_vars = c('", paste(parsed$by_vars, collapse="','"), "')"))
  if (!is.na(r_if_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))

  r_code = paste0("data = scmd_collapse(", paste(args, collapse = ", "), ")")
  r_code = paste0(r_code, "\nassign(\"has_original_order_idx\", TRUE, envir = stata2r_env)")

  return(r_code)
}

# 3. Runtime Execution Phase
# 3. Runtime Execution Phase
scmd_collapse = function(data, agg_exprs_list, group_vars = character(0), r_if_cond = NA_character_) {
  restore.point("scmd_collapse")
  if (!is.na(r_if_cond) && r_if_cond != "") data = data[s2r_eval_cond(data, r_if_cond, envir = parent.frame()), , drop = FALSE]

  pipe_el = c("data")
  group_vars_actual = expand_varlist(paste(group_vars, collapse=" "), names(data))
  if (length(group_vars_actual) > 0) pipe_el = c(pipe_el, paste0("collapse::fgroup_by(", paste(group_vars_actual, collapse=", "), ")"))

  agg_str = paste(sprintf("`%s` = %s", names(agg_exprs_list), unlist(agg_exprs_list)), collapse = ", ")
  pipe_el = c(pipe_el, paste0("collapse::fsummarise(", agg_str, ")"))
  if (length(group_vars_actual) > 0) pipe_el = c(pipe_el, "collapse::fungroup()")

  data = eval(parse(text = paste(pipe_el, collapse = " %>% ")), envir = list(data = data), enclos = parent.frame())
  data$stata2r_original_order_idx = seq_len(nrow(data))
  return(data)
}
