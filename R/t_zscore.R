# FILE: R/t_zscore.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_zscore = function(rest_of_cmd) {
  restore.point("s2r_p_zscore")
  parsed = s2r_parse_if_in(rest_of_cmd)

  options_match = stringi::stri_match_first_regex(parsed$base_str, ",\\s*(.*)$")
  options_str = NA_character_
  varlist = parsed$base_str

  if (!is.na(options_match[1,1])) {
    options_str = stringi::stri_trim_both(options_match[1,2])
    varlist = stringi::stri_trim_both(stringi::stri_replace_last_regex(parsed$base_str, ",\\s*(.*)$", ""))
  }

  stub = NA_character_
  if (!is.na(options_str)) {
    stub_match = stringi::stri_match_first_regex(options_str, "\\bstub\\s*\\(([^)]+)\\)")
    if (!is.na(stub_match[1,1])) {
      stub = stringi::stri_trim_both(stub_match[1,2])
    }
  }

  list(varlist = varlist, if_str = parsed$if_str, in_str = parsed$in_str, stub = stub)
}

# 2. Code Generation Phase: Emit R code
t_zscore = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_zscore")
  parsed = s2r_p_zscore(rest_of_cmd)

  if (is.na(parsed$varlist) || parsed$varlist == "") return(paste0("# Failed to parse zscore command: ", rest_of_cmd))

  r_if_cond = NA_character_
  if (!is.na(parsed$if_str) && parsed$if_str != "") {
    r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, list(is_by_group=FALSE))
  }
  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  group_vars_list_bare = character(0)
  if (isTRUE(cmd_obj$is_by_prefix) && !is.na(cmd_obj$by_group_vars) && cmd_obj$by_group_vars != "") {
    group_vars_list = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
    group_vars_list_bare = group_vars_list[!is.na(group_vars_list) & group_vars_list != ""]
  }

  args = c("data = data", paste0("varlist_str = ", quote_for_r_literal(parsed$varlist)))
  if (!is.na(parsed$stub)) args = c(args, paste0("stub = ", quote_for_r_literal(parsed$stub)))
  if (!is.na(r_if_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  if (!is.na(r_in_range)) args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))
  if (length(group_vars_list_bare) > 0) args = c(args, paste0("group_vars = c('", paste(group_vars_list_bare, collapse = "','"), "')"))

  r_code = paste0("data = scmd_zscore(", paste(args, collapse = ", "), ")")
  return(r_code)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_zscore = function(data, varlist_str, stub = NA_character_, r_if_cond = NA_character_, r_in_range = NA_character_, group_vars = character(0)) {
  restore.point("scmd_zscore")

  vars_actual = expand_varlist(varlist_str, names(data))
  if (length(vars_actual) == 0) stop("scmd_zscore: no variables matched.")

  prefix = if (is.na(stub)) "z_" else stub

  mask = rep(TRUE, nrow(data))
  if (!is.na(r_if_cond) && r_if_cond != "") {
    r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))
    mask = mask & s2r_eval_cond(data, r_if_cond, envir = parent.frame())
  }
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask = rep(FALSE, nrow(data))
    in_mask[idx] = TRUE
    mask = mask & in_mask
  }

  group_vars_actual = expand_varlist(paste(group_vars, collapse=" "), names(data))

  data$.stata_temp_mask = mask

  pipe_el = c("data")
  if (length(group_vars_actual) > 0) {
    pipe_el = c(pipe_el, paste0("dplyr::group_by(!!!dplyr::syms(c('", paste(group_vars_actual, collapse="','"), "')))"))
  }

  for (v in vars_actual) {
    new_v = paste0(prefix, v)
    if (new_v %in% names(data)) stop(paste0("scmd_zscore: variable ", new_v, " already exists."))

    # Generate the command to calculate the z-score.
    # We replace any elements outside the mask bounds with NA_real_ before taking the group-wise
    # mean and standard deviation to accurately mirror Stata filtering conventions.
    mutate_str = paste0(
      "dplyr::mutate(`", new_v, "` = dplyr::if_else(.stata_temp_mask, ",
      "(as.numeric(`", v, "`) - mean(replace(as.numeric(`", v, "`), !.stata_temp_mask, NA_real_), na.rm = TRUE)) / ",
      "stats::sd(replace(as.numeric(`", v, "`), !.stata_temp_mask, NA_real_), na.rm = TRUE), ",
      "NA_real_))"
    )

    pipe_el = c(pipe_el, mutate_str, paste0("dplyr::mutate(`", new_v, "` = dplyr::if_else(is.infinite(`", new_v, "`) | is.nan(`", new_v, "`), NA_real_, `", new_v, "`))"))
  }

  if (length(group_vars_actual) > 0) pipe_el = c(pipe_el, "dplyr::ungroup()")

  eval_env = new.env(parent = parent.frame())
  eval_env$data = data
  data = eval(parse(text = paste(pipe_el, collapse = " %>% ")), envir = eval_env)

  data$.stata_temp_mask = NULL
  return(data)
}
