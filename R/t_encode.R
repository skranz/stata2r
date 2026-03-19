# FILE: R/t_encode.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_encode = function(rest_of_cmd) {
  restore.point("s2r_p_encode")
  parsed = s2r_parse_if_in(rest_of_cmd)

  options_match = stringi::stri_match_first_regex(parsed$base_str, ",\\s*(.*)$")
  if (is.na(options_match[1,1])) return(list(varname = NA_character_))

  options_str = stringi::stri_trim_both(options_match[1,2])
  varname = stringi::stri_trim_both(stringi::stri_replace_last_regex(parsed$base_str, ",\\s*(.*)$", ""))

  gen_var = NA_character_
  gen_match = stringi::stri_match_first_regex(options_str, "\\b(?:gen|generate)\\s*\\(([^)]+)\\)")
  if (!is.na(gen_match[1,1])) {
    gen_var = stringi::stri_split_regex(stringi::stri_trim_both(gen_match[1,2]), "\\s+")[[1]][1]
  }

  list(varname = varname, if_str = parsed$if_str, in_str = parsed$in_str, gen_var = gen_var)
}

# 2. Code Generation Phase: Emit R code
t_encode = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_encode")
  parsed = s2r_p_encode(rest_of_cmd)
  if (is.na(parsed$varname) || is.na(parsed$gen_var)) return(paste0("# encode requires varname and gen() option"))

  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, list(is_by_group=FALSE))
  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  args = c("data = data", paste0("varname = ", quote_for_r_literal(parsed$varname)), paste0("gen_var = ", quote_for_r_literal(parsed$gen_var)))
  if (!is.na(r_if_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  if (!is.na(r_in_range)) args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))

  return(paste0("data = scmd_encode(", paste(args, collapse = ", "), ")"))
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_encode = function(data, varname, gen_var, r_if_cond = NA_character_, r_in_range = NA_character_) {
  restore.point("scmd_encode")
  var_actual = expand_varlist(varname, names(data))[1]

  mask = rep(TRUE, nrow(data))
  if (!is.na(r_if_cond) && r_if_cond != "") mask = mask & s2r_eval_cond(data, r_if_cond)
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask = rep(FALSE, nrow(data))
    in_mask[idx] = TRUE
    mask = mask & in_mask
  }

  src = data[[var_actual]]
  uq_vals = sort(unique(src[!is.na(src)]))
  num_vals = match(src, uq_vals)
  labels_map = stats::setNames(as.numeric(1:length(uq_vals)), uq_vals)

  encoded = haven::labelled(as.integer(num_vals), labels = labels_map)

  if (!(gen_var %in% names(data))) data[[gen_var]] = NA_integer_
  data[[gen_var]] = dplyr::if_else(mask, encoded, data[[gen_var]])

  return(data)
}
