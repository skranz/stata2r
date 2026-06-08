# FILE: R/t_carryforward.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_carryforward = function(rest_of_cmd) {
  restore.point("s2r_p_carryforward")
  parsed = s2r_parse_if_in(rest_of_cmd)

  options_match = stringi::stri_match_first_regex(parsed$base_str, ",\\s*(.*)$")
  options_str = NA_character_
  varlist = parsed$base_str

  if (!is.na(options_match[1,1])) {
    options_str = stringi::stri_trim_both(options_match[1,2])
    varlist = stringi::stri_trim_both(stringi::stri_replace_last_regex(parsed$base_str, ",\\s*(.*)$", ""))
  }

  gen_vars = NA_character_
  is_replace = FALSE
  backwards = FALSE

  if (!is.na(options_str)) {
    gen_opt = stringi::stri_match_first_regex(options_str, "\\b(?:gen|generate)\\s*\\(([^)]+)\\)")
    if (!is.na(gen_opt[1,1])) gen_vars = stringi::stri_trim_both(gen_opt[1,2])

    if (fast_coalesce(stringi::stri_detect_regex(options_str, "\\breplace\\b"), FALSE)) {
      is_replace = TRUE
    }
    if (fast_coalesce(stringi::stri_detect_regex(options_str, "\\bbackwards\\b"), FALSE)) {
      backwards = TRUE
    }
  }

  list(varlist = varlist, if_str = parsed$if_str, in_str = parsed$in_str,
       gen_vars = gen_vars, is_replace = is_replace, backwards = backwards)
}

# 2. Code Generation Phase: Emit R code
t_carryforward = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_carryforward")
  parsed = s2r_p_carryforward(rest_of_cmd)

  if (is.na(parsed$varlist) || parsed$varlist == "") return("# Failed to parse carryforward command")

  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, list(is_by_group = FALSE))
  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  group_vars_list_bare = character(0)
  if (isTRUE(cmd_obj$is_by_prefix) && !is.na(cmd_obj$by_group_vars) && cmd_obj$by_group_vars != "") {
    group_vars_list = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
    group_vars_list_bare = group_vars_list[!is.na(group_vars_list) & group_vars_list != ""]
  }

  args = c("data = data", paste0("varlist_str = ", quote_for_r_literal(parsed$varlist)), paste0("is_replace = ", parsed$is_replace))
  if (!is.na(parsed$gen_vars)) args = c(args, paste0("gen_vars_str = ", quote_for_r_literal(parsed$gen_vars)))
  args = c(args, paste0("backwards = ", parsed$backwards))
  if (!is.na(r_if_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  if (!is.na(r_in_range)) args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))
  if (length(group_vars_list_bare) > 0) args = c(args, paste0("group_vars = c('", paste(group_vars_list_bare, collapse = "','"), "')"))

  r_code = paste0("data = scmd_carryforward(", paste(args, collapse = ", "), ")")
  return(r_code)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_carryforward = function(data, varlist_str, is_replace, gen_vars_str = NA_character_, backwards = FALSE, r_if_cond = NA_character_, r_in_range = NA_character_, group_vars = character(0)) {
  restore.point("scmd_carryforward")

  vars_actual = expand_varlist(varlist_str, names(data))
  if (length(vars_actual) == 0) stop("scmd_carryforward: no variables matched.")

  new_vars = vars_actual
  if (!is_replace) {
    if (is.na(gen_vars_str)) stop("scmd_carryforward: must specify gen() or replace.")
    new_vars = stringi::stri_split_regex(gen_vars_str, "\\s+")[[1]]
    new_vars = new_vars[new_vars != ""]
    if (length(new_vars) != length(vars_actual)) {
      stop("scmd_carryforward: generate() requires same number of new variables as old variables.")
    }
  }

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
  data$.stata_temp_mask = mask

  direction = if (backwards) "up" else "down"

  group_vars_actual = expand_varlist(paste(group_vars, collapse=" "), names(data))

  pipe_el = c("data")
  if (length(group_vars_actual) > 0) {
    pipe_el = c(pipe_el, paste0("dplyr::group_by(!!!dplyr::syms(c('", paste(group_vars_actual, collapse="','"), "')))"))
  }

  for (i in seq_along(vars_actual)) {
    old_var = vars_actual[i]
    new_var = new_vars[i]

    if (!is_replace) {
      pipe_el = c(pipe_el, paste0("dplyr::mutate(`", new_var, "` = `", old_var, "`)"))
    }

    pipe_el = c(pipe_el, paste0("dplyr::mutate(`", new_var, "` = if(is.character(`", new_var, "`)) dplyr::if_else(`", new_var, "` == '', NA_character_, `", new_var, "`) else `", new_var, "`)"))

    fill_cmd = paste0("tidyr::fill(`", new_var, "`, .direction = '", direction, "')")
    pipe_el = c(pipe_el, fill_cmd)

    pipe_el = c(pipe_el, paste0("dplyr::mutate(`", new_var, "` = dplyr::if_else(.stata_temp_mask, `", new_var, "`, `", old_var, "`))"))

    pipe_el = c(pipe_el, paste0("dplyr::mutate(`", new_var, "` = if(is.character(`", new_var, "`)) dplyr::if_else(is.na(`", new_var, "`), '', `", new_var, "`) else `", new_var, "`)"))
  }

  if (length(group_vars_actual) > 0) pipe_el = c(pipe_el, "dplyr::ungroup()")

  data = eval(parse(text = paste(pipe_el, collapse = " %>% ")), envir = list(data = data), enclos = parent.frame())
  data$.stata_temp_mask = NULL

  return(data)
}
