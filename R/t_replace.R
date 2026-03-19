# FILE: R/t_replace.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_replace = function(rest_of_cmd) {
  restore.point("s2r_p_replace")
  explicit_type_match = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*(?:byte|int|long|float|double|str\\d+|strL)\\s+")
  declared_type_str = if (!is.na(explicit_type_match[1,1])) explicit_type_match[1,2] else NA_character_

  rest_no_type = stringi::stri_replace_first_regex(rest_of_cmd, "^\\s*(?:byte|int|long|float|double|str\\d+|strL)\\s+", "")
  match = stringi::stri_match_first_regex(rest_no_type, "^\\s*([^=\\s]+)\\s*=\\s*(.*?)(?:\\s+if\\s+(.*))?$")

  list(
    declared_type = declared_type_str,
    var_to_replace = if (!is.na(match[1,1])) stringi::stri_trim_both(match[1,2]) else NA_character_,
    stata_expr = if (!is.na(match[1,1])) stringi::stri_trim_both(match[1,3]) else NA_character_,
    if_cond = if (!is.na(match[1,1])) stringi::stri_trim_both(match[1,4]) else NA_character_
  )
}

# 2. Code Generation Phase: Emit R code
t_replace = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_replace")
  parsed = s2r_p_replace(rest_of_cmd)
  if (is.na(parsed$var_to_replace)) return(paste0("# Failed to parse replace command: ", rest_of_cmd))

  current_context = list(is_by_group = cmd_obj$is_by_prefix && length(cmd_obj$by_group_vars) > 0 && !is.na(cmd_obj$by_group_vars[1]))
  r_expr = translate_stata_expression_with_r_values(parsed$stata_expr, line_num, cmd_df, current_context)
  if (is.na(r_expr)) r_expr = "NA_real_"

  r_if_cond = NA_character_
  if (!is.na(parsed$if_cond) && parsed$if_cond != "") {
    r_if_cond = translate_stata_expression_with_r_values(parsed$if_cond, line_num, cmd_df, list(is_by_group = FALSE))
  }

  group_vars_list_bare = character(0)
  if (current_context$is_by_group) {
    group_vars_list = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
    group_vars_list_bare = group_vars_list[!is.na(group_vars_list) & group_vars_list != ""]
  }

  is_string = sfun_is_stata_expression_string_typed(parsed$stata_expr)
  force_integer = FALSE
  if (!is.na(parsed$declared_type)) {
    is_string = stringi::stri_startswith_fixed(parsed$declared_type, "str")
    force_integer = parsed$declared_type %in% c("byte", "int", "long")
  }

  args = c("data = data", paste0("var_to_replace = ", quote_for_r_literal(parsed$var_to_replace)), paste0("r_expr_str = ", quote_for_r_literal(r_expr)))
  if (!is.na(r_if_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))

  if (length(group_vars_list_bare) > 0) args = c(args, paste0("group_vars = c('", paste(group_vars_list_bare, collapse="','"), "')"))
  args = c(args, paste0("is_string = ", is_string), paste0("force_integer = ", force_integer))

  return(paste0("data = scmd_replace(", paste(args, collapse = ", "), ")"))
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_replace = function(data, var_to_replace, r_expr_str, r_if_cond = NA_character_, group_vars = character(0), is_string = FALSE, force_integer = FALSE) {
  restore.point("scmd_replace")

  var_actual = expand_varlist(var_to_replace, names(data))[1]

  expr_val = r_expr_str
  if (is_string) {
    if (expr_val == "NA_real_") expr_val = '""' else expr_val = paste0("as.character(", expr_val, ")")
  } else {
    if (force_integer) expr_val = paste0("as.integer(", expr_val, ")") else expr_val = paste0("as.numeric(", expr_val, ")")
  }

  if (!is.na(r_if_cond) && r_if_cond != "") {
    expr_val = paste0("dplyr::if_else((dplyr::coalesce(as.numeric(", r_if_cond, "), 0) != 0), ", expr_val, ", data$`", var_actual, "`)")
  }

  pipe_el = c("data")
  if (length(group_vars) > 0) pipe_el = c(pipe_el, paste0("dplyr::group_by(!!!dplyr::syms(c('", paste(group_vars, collapse="','"), "')))"))
  pipe_el = c(pipe_el, paste0("dplyr::mutate(`", var_actual, "` = ", expr_val, ")"))
  if (length(group_vars) > 0) pipe_el = c(pipe_el, "dplyr::ungroup()")

  eval(parse(text = paste(pipe_el, collapse = " %>% ")))
}
