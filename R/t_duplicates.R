# FILE: R/t_duplicates.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_duplicates = function(rest_of_cmd) {
  restore.point("s2r_p_duplicates")
  parts = stringi::stri_split_regex(stringi::stri_trim_both(rest_of_cmd), "\\s+", n=2)[[1]]
  subcmd = parts[1]
  rest = if(length(parts) > 1) parts[2] else ""

  parsed = s2r_parse_if_in(rest)

  options_match = stringi::stri_match_first_regex(parsed$base_str, ",\\s*(.*)$")
  options_str = NA_character_
  varlist = parsed$base_str
  if (!is.na(options_match[1,1])) {
    options_str = stringi::stri_trim_both(options_match[1,2])
    varlist = stringi::stri_trim_both(stringi::stri_replace_last_regex(parsed$base_str, ",\\s*(.*)$", ""))
  }

  gen_var = NA_character_
  if (!is.na(options_str)) {
    gen_opt = stringi::stri_match_first_regex(options_str, "\\bgen\\s*\\(([^)]+)\\)")
    if (!is.na(gen_opt[1,1])) gen_var = stringi::stri_split_regex(stringi::stri_trim_both(gen_opt[1,2]), "\\s+")[[1]][1]
  }

  list(subcommand = subcmd, varlist = varlist, if_str = parsed$if_str, in_str = parsed$in_str, gen_var = gen_var)
}

# 2. Code Generation Phase: Emit R code
t_duplicates = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_duplicates")
  parsed = s2r_p_duplicates(rest_of_cmd)

  if (is.na(parsed$subcommand) || !(parsed$subcommand %in% c("drop", "tag", "list"))) {
    return(paste0("# Failed to parse duplicates subcommand: ", rest_of_cmd))
  }

  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, list(is_by_group=FALSE))
  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  args = c("data = data", paste0("subcommand = ", quote_for_r_literal(parsed$subcommand)))
  if (!is.na(parsed$varlist) && parsed$varlist != "") args = c(args, paste0("varlist_str = ", quote_for_r_literal(parsed$varlist)))
  if (!is.na(parsed$gen_var)) args = c(args, paste0("gen_var = ", quote_for_r_literal(parsed$gen_var)))
  if (!is.na(r_if_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  if (!is.na(r_in_range)) args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))

  r_code = paste0("data = scmd_duplicates(", paste(args, collapse = ", "), ")")
  if (parsed$subcommand == "drop") {
    r_code = paste0(r_code, "\nif (isTRUE(stata2r_env$has_original_order_idx)) { data = dplyr::mutate(data, stata2r_original_order_idx = dplyr::row_number()) }")
  }

  return(r_code)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_duplicates = function(data, subcommand, varlist_str = NA_character_, gen_var = NA_character_, r_if_cond = NA_character_, r_in_range = NA_character_) {
  restore.point("scmd_duplicates")
  mask = rep(TRUE, nrow(data))
  if (!is.na(r_if_cond) && r_if_cond != "") mask = mask & s2r_eval_cond(data, r_if_cond)
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask = rep(FALSE, nrow(data))
    in_mask[idx] = TRUE
    mask = mask & in_mask
  }

  cols = names(data)
  if (!is.na(varlist_str) && varlist_str != "") {
    cols = expand_varlist(varlist_str, names(data))
  }

  is_dup = base::duplicated(data[, cols, drop = FALSE], fromLast = FALSE)

  if (subcommand == "drop") {
    data = data[!(is_dup & mask), , drop = FALSE]
    rownames(data) = NULL
  } else if (subcommand == "tag") {
    if (is.na(gen_var)) stop("duplicates tag requires gen_var")
    is_first = !is_dup
    data[[gen_var]] = dplyr::if_else(is_first & mask, 1, 0)
  } else if (subcommand == "list") {
    print(data[is_dup & mask, , drop = FALSE])
  }

  return(data)
}
