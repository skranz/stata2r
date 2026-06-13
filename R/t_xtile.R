# FILE: R/t_xtile.R

# 1. Parsing Phase
s2r_p_xtile = function(rest_of_cmd) {
  restore.point("s2r_p_xtile")
  # Example: faminc_quart=faminc_06, nq(4)
  # or: faminc_quart = faminc_06 if something, nq(4)

  parts = stringi::stri_split_fixed(rest_of_cmd, "=", n=2)[[1]]
  if (length(parts) < 2) return(list(new_var=NA_character_))

  new_var = stringi::stri_trim_both(parts[1])
  right_side = stringi::stri_trim_both(parts[2])

  # parse if/in from right_side, but note that options come after a comma
  comma_split = stringi::stri_split_fixed(right_side, ",", n=2)[[1]]
  expr_and_conds = stringi::stri_trim_both(comma_split[1])
  options_str = if (length(comma_split) > 1) stringi::stri_trim_both(comma_split[2]) else NA_character_

  parsed = s2r_parse_if_in(expr_and_conds)
  stata_expr = parsed$base_str

  nq = NA_real_
  cutpoints = NA_character_

  if (!is.na(options_str)) {
    nq_match = stringi::stri_match_first_regex(options_str, "\\b(?:nq|nquantiles)\\s*\\(\\s*(\\d+)\\s*\\)")
    if (!is.na(nq_match[1,1])) nq = as.numeric(nq_match[1,2])

    cut_match = stringi::stri_match_first_regex(options_str, "\\b(?:cut|cutpoints)\\s*\\(\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\)")
    if (!is.na(cut_match[1,1])) cutpoints = stringi::stri_trim_both(cut_match[1,2])
  }

  if (is.na(nq) && is.na(cutpoints)) nq = 2 # default in Stata

  list(new_var = new_var, stata_expr = stata_expr, if_str = parsed$if_str, in_str = parsed$in_str, nq = nq, cutpoints = cutpoints)
}

# 2. Code Generation Phase
t_xtile = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_xtile")
  parsed = s2r_p_xtile(rest_of_cmd)
  if (is.na(parsed$new_var)) return(paste0("# Failed to parse xtile command: ", rest_of_cmd))

  r_expr = translate_stata_expression_with_r_values(parsed$stata_expr, line_num, cmd_df, list(is_by_group = FALSE))
  r_if_cond = NA_character_
  if (!is.na(parsed$if_str) && parsed$if_str != "") {
    r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, list(is_by_group = FALSE))
  }
  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  group_vars_list_bare = character(0)
  if (isTRUE(cmd_obj$is_by_prefix) && !is.na(cmd_obj$by_group_vars) && cmd_obj$by_group_vars != "") {
    group_vars_list = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
    group_vars_list_bare = group_vars_list[!is.na(group_vars_list) & group_vars_list != ""]
  }

  args = c("data = data", paste0("new_var = ", quote_for_r_literal(parsed$new_var)), paste0("r_expr_str = ", quote_for_r_literal(r_expr)))
  if (!is.na(parsed$nq)) args = c(args, paste0("nq = ", parsed$nq))
  if (!is.na(parsed$cutpoints)) args = c(args, paste0("cutpoints = ", quote_for_r_literal(parsed$cutpoints)))
  if (!is.na(r_if_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  if (!is.na(r_in_range)) args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))
  if (length(group_vars_list_bare) > 0) args = c(args, paste0("group_vars = c('", paste(group_vars_list_bare, collapse="','"), "')"))

  return(paste0("data = scmd_xtile(", paste(args, collapse = ", "), ")"))
}

# 3. Runtime Execution Phase
scmd_xtile = function(data, new_var, r_expr_str, nq = NA_real_, cutpoints = NA_character_, r_if_cond = NA_character_, r_in_range = NA_character_, group_vars = character(0)) {
  restore.point("scmd_xtile")

  r_expr_str = resolve_abbrevs_in_expr(r_expr_str, names(data))
  r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))

  mask = rep(TRUE, nrow(data))
  if (!is.na(r_if_cond) && r_if_cond != "") {
    mask = mask & s2r_eval_cond(data, r_if_cond, envir = parent.frame())
  }
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask = rep(FALSE, nrow(data))
    in_mask[idx] = TRUE
    mask = mask & in_mask
  }

  data$.stata_temp_mask = mask
  group_vars_actual = expand_varlist(paste(group_vars, collapse=" "), names(data))

  eval_env = s2r_stata_env(parent.frame())
  eval_env$data = data

  pipe_el = c("data", paste0("dplyr::mutate(.xtile_val = as.numeric(", r_expr_str, "))"))
  tmp_data = eval(parse(text = paste(pipe_el, collapse = " %>% ")), envir = eval_env)

  calc_xtile = function(df) {
    val = df$.xtile_val
    valid = df$.stata_temp_mask & !is.na(val)

    res = rep(NA_integer_, nrow(df))
    if (!any(valid)) return(res)

    if (!is.na(cutpoints) && cutpoints != "") {
      cut_var = expand_varlist(cutpoints, names(df))[1]
      if (is.na(cut_var)) stop(paste0("scmd_xtile: cutpoints variable '", cutpoints, "' not found"))

      cuts = unique(sort(df[[cut_var]], na.last=NA))
      if (length(cuts) > 0) {
        breaks = c(-Inf, cuts, Inf)
        res[valid] = as.integer(cut(val[valid], breaks = breaks, right = TRUE, include.lowest = TRUE, labels = FALSE))
      }
    } else if (!is.na(nq)) {
      probs = seq(0, 1, length.out = nq + 1)
      # type=2 evaluates closest to typical step logic in non-survey xtile behaviour
      breaks = stats::quantile(val[valid], probs, na.rm=TRUE, type=2)
      breaks[1] = -Inf
      breaks[length(breaks)] = Inf
      breaks = unique(breaks)

      if (length(breaks) > 1) {
        res[valid] = as.integer(cut(val[valid], breaks = breaks, right = TRUE, include.lowest = TRUE, labels = FALSE))
      } else {
        res[valid] = 1L
      }
    }

    res
  }

  if (length(group_vars_actual) > 0) {
    res_list = split(tmp_data, tmp_data[, group_vars_actual, drop=FALSE])
    res_vec = unsplit(lapply(res_list, calc_xtile), tmp_data[, group_vars_actual, drop=FALSE])
    tmp_data[[new_var]] = res_vec
  } else {
    tmp_data[[new_var]] = calc_xtile(tmp_data)
  }

  data[[new_var]] = tmp_data[[new_var]]
  data$.stata_temp_mask = NULL

  return(data)
}
