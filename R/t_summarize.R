# FILE: R/t_summarize.R

# 1. Parsing Phase
s2r_p_summarize = function(rest_of_cmd) {
  restore.point("s2r_p_summarize")
  parts = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*([^,]*?)(?:,\\s*(.*))?$")
  parsed = s2r_parse_if_in(stringi::stri_trim_both(parts[1,2]))

  vars = stringi::stri_split_regex(stringi::stri_trim_both(parsed$base_str), "\\s+")[[1]]
  list(varlist = vars[vars != ""], if_str = parsed$if_str, options = parts[1,3])
}

# 2. Code Generation Phase
t_summarize = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_summarize")
  needed_r = unlist(cmd_obj$r_results_needed)
  if (length(needed_r) == 0) return(paste0("# summarize at line ", line_num, " is no-op (no r() used)."))

  parsed = s2r_p_summarize(rest_of_cmd)

  var_for_r = NA_character_
  if (length(parsed$varlist) > 0) var_for_r = parsed$varlist[length(parsed$varlist)]

  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, list(is_by_group=FALSE))

  needed_r_str = paste0("c('", paste(needed_r, collapse="','"), "')")

  args = c("data = data", paste0("needed_r = ", needed_r_str))
  if (!is.na(var_for_r)) args = c(args, paste0("var_for_r = ", quote_for_r_literal(var_for_r)))
  if (!is.na(r_if_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))

  r_code = paste0("res_L", line_num, " = scmd_summarize(", paste(args, collapse = ", "), ")")

  pref = paste0("\nstata_r_val_L", line_num, "_")
  if ("r(N)" %in% needed_r) r_code = paste0(r_code, pref, "N = res_L", line_num, "$r_N")
  if ("r(mean)" %in% needed_r) r_code = paste0(r_code, pref, "mean = res_L", line_num, "$r_mean")
  if ("r(sd)" %in% needed_r) r_code = paste0(r_code, pref, "sd = res_L", line_num, "$r_sd")
  if ("r(min)" %in% needed_r) r_code = paste0(r_code, pref, "min = res_L", line_num, "$r_min")
  if ("r(max)" %in% needed_r) r_code = paste0(r_code, pref, "max = res_L", line_num, "$r_max")
  if ("r(sum)" %in% needed_r) r_code = paste0(r_code, pref, "sum = res_L", line_num, "$r_sum")
  if ("r(p50)" %in% needed_r) r_code = paste0(r_code, pref, "p50 = res_L", line_num, "$r_p50")

  return(r_code)
}

# 3. Runtime Execution Phase
scmd_summarize = function(data, needed_r, var_for_r = NA_character_, r_if_cond = NA_character_) {
  restore.point("scmd_summarize")
  if (!is.na(r_if_cond) && r_if_cond != "") data = data[s2r_eval_cond(data, r_if_cond), , drop=FALSE]

  res = list()
  if (is.na(var_for_r)) {
    if ("r(N)" %in% needed_r) res$r_N = nrow(data)
  } else {
    v = expand_varlist(var_for_r, names(data))[1]
    col = data[[v]]
    if ("r(N)" %in% needed_r) res$r_N = sum(!is.na(col))
    if ("r(mean)" %in% needed_r) res$r_mean = mean(col, na.rm=TRUE)
    if ("r(sd)" %in% needed_r) res$r_sd = sd(col, na.rm=TRUE)
    if ("r(min)" %in% needed_r) res$r_min = min(col, na.rm=TRUE)
    if ("r(max)" %in% needed_r) res$r_max = max(col, na.rm=TRUE)
    if ("r(sum)" %in% needed_r) res$r_sum = sum(col, na.rm=TRUE)
    if ("r(p50)" %in% needed_r) res$r_p50 = median(col, na.rm=TRUE)
  }
  return(res)
}
