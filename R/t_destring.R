# FILE: R/t_destring.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_destring = function(rest_of_cmd) {
  restore.point("s2r_p_destring")
  parsed = s2r_parse_if_in(rest_of_cmd)

  options_match = stringi::stri_match_first_regex(parsed$base_str, ",\\s*(.*)$")
  if (is.na(options_match[1,1])) return(list(varlist = NA_character_))

  options_str = stringi::stri_trim_both(options_match[1,2])
  varlist_str = stringi::stri_trim_both(stringi::stri_replace_last_regex(parsed$base_str, ",\\s*(.*)$", ""))

  is_replace = dplyr::coalesce(stringi::stri_detect_fixed(options_str, "replace"), FALSE)
  gen_vars = NA_character_
  if (!is_replace) {
    gen_opt = stringi::stri_match_first_regex(options_str, "\\bgenerate\\s*\\(([^)]+)\\)")
    if (!is.na(gen_opt[1,1])) gen_vars = stringi::stri_trim_both(gen_opt[1,2])
  }

  list(varlist = varlist_str, if_str = parsed$if_str, in_str = parsed$in_str,
       is_replace = is_replace, gen_vars = gen_vars)
}

# 2. Code Generation Phase: Emit R code
t_destring = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_destring")
  parsed = s2r_p_destring(rest_of_cmd)
  if (is.na(parsed$varlist) || parsed$varlist == "") return(paste0("# destring command requires varlist and options"))

  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, list(is_by_group=FALSE))
  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  args = c("data = data", paste0("varlist_str = ", quote_for_r_literal(parsed$varlist)), paste0("is_replace = ", parsed$is_replace))
  if (!is.na(parsed$gen_vars)) args = c(args, paste0("gen_vars_str = ", quote_for_r_literal(parsed$gen_vars)))
  if (!is.na(r_if_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  if (!is.na(r_in_range)) args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))

  return(paste0("data = scmd_destring(", paste(args, collapse = ", "), ")"))
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_destring = function(data, varlist_str, is_replace, gen_vars_str = NA_character_, r_if_cond = NA_character_, r_in_range = NA_character_) {
  restore.point("scmd_destring")
  vars_actual = expand_varlist(varlist_str, names(data))

  if (is_replace) {
    new_vars = vars_actual
  } else {
    new_vars = stringi::stri_split_regex(gen_vars_str, "\\s+")[[1]]
    new_vars = new_vars[new_vars != ""]
    if (length(new_vars) != length(vars_actual)) {
      stop("scmd_destring: generate() requires same number of new variables as old variables.")
    }
  }

  mask = rep(TRUE, nrow(data))
  if (!is.na(r_if_cond) && r_if_cond != "") mask = mask & s2r_eval_cond(data, r_if_cond)
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask = rep(FALSE, nrow(data))
    in_mask[idx] = TRUE
    mask = mask & in_mask
  }

  for (i in seq_along(vars_actual)) {
    old_v = vars_actual[i]
    new_v = new_vars[i]
    destrung = suppressWarnings(readr::parse_number(as.character(data[[old_v]])))

    if (is_replace) {
      data[[new_v]] = dplyr::if_else(mask, destrung, data[[old_v]])
    } else {
      res = rep(NA_real_, nrow(data))
      res[mask] = destrung[mask]
      data[[new_v]] = res
    }
  }

  return(data)
}
