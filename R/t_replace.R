# FILE: R/t_replace.R

# 1. Parsing Phase: Extract Stata syntax components
# 1. Parsing Phase: Extract Stata syntax components
# 1. Parsing Phase: Extract Stata syntax components
s2r_p_replace = function(rest_of_cmd) {
  restore.point("s2r_p_replace")
  explicit_type_match = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*(byte|int|long|float|double|str\\d+|strL)\\s+")
  declared_type_str = if (!is.na(explicit_type_match[1,1])) explicit_type_match[1,2] else NA_character_

  rest_no_type = stringi::stri_replace_first_regex(rest_of_cmd, "^\\s*(?:byte|int|long|float|double|str\\d+|strL)\\s+", "")

  parts = stringi::stri_split_fixed(rest_no_type, "=", n=2)[[1]]
  if (length(parts) < 2) {
    return(list(
      declared_type = declared_type_str,
      var_to_replace = NA_character_,
      stata_expr = NA_character_,
      if_cond = NA_character_,
      in_str = NA_character_
    ))
  }

  var_to_replace = stringi::stri_trim_both(parts[1])
  right_side = stringi::stri_trim_both(parts[2])

  parsed = s2r_parse_if_in(right_side)

  list(
    declared_type = declared_type_str,
    var_to_replace = var_to_replace,
    stata_expr = parsed$base_str,
    if_cond = parsed$if_str,
    in_str = parsed$in_str
  )
}

# 2. Code Generation Phase: Emit R code
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

  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

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
  if (!is.na(r_in_range)) args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))

  if (length(group_vars_list_bare) > 0) args = c(args, paste0("group_vars = c('", paste(group_vars_list_bare, collapse="','"), "')"))
  args = c(args, paste0("is_string = ", is_string), paste0("force_integer = ", force_integer))

  return(paste0("data = scmd_replace(", paste(args, collapse = ", "), ")"))
}

# 3. Runtime Execution Phase: Evaluate against actual data
# 3. Runtime Execution Phase: Evaluate against actual data
# 3. Runtime Execution Phase: Evaluate against actual data
# 3. Runtime Execution Phase: Evaluate against actual data
scmd_replace = function(data, var_to_replace, r_expr_str, r_if_cond = NA_character_, r_in_range = NA_character_, group_vars = character(0), is_string = FALSE, force_integer = FALSE) {
  restore.point("scmd_replace")

  var_actual = expand_varlist(var_to_replace, names(data))[1]
  r_expr_str = resolve_abbrevs_in_expr(r_expr_str, names(data))
  r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))

  # If the target is character, we safely flag it as string to prevent NA coercion
  target_is_char = is.character(data[[var_actual]])
  is_string = is_string || target_is_char

  mask_expr = ".stata_temp_mask"
  if (!is.na(r_if_cond) && r_if_cond != "") {
    mask_expr = paste0("(.stata_temp_mask & fast_coalesce(as.numeric(", r_if_cond, "), 0) != 0)")
  }

  if (r_expr_str == "NA_real_") {
    if (is_string) {
       expr_body = paste0("dplyr::if_else(", mask_expr, ", '', `", var_actual, "`)")
    } else {
       expr_body = paste0("dplyr::if_else(", mask_expr, ", NA_real_, `", var_actual, "`)")
    }
  } else {
    if (is_string) {
        expr_body = paste0("{ .val = as.character(", r_expr_str, "); dplyr::if_else(", mask_expr, ", .val, `", var_actual, "`) }")
    } else if (force_integer) {
        expr_body = paste0("{ .val = as.integer(", r_expr_str, "); dplyr::if_else(", mask_expr, ", .val, `", var_actual, "`) }")
    } else {
        expr_body = paste0("{ .val = ", r_expr_str, "; if(is.character(.val)) { dplyr::if_else(", mask_expr, ", .val, `", var_actual, "`) } else { .val = as.numeric(.val); dplyr::if_else(", mask_expr, ", .val, `", var_actual, "`) } }")
    }
  }

  # Compute in-range mask globally
  in_mask = rep(TRUE, nrow(data))
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask_vec = rep(FALSE, nrow(data))
    in_mask_vec[idx] = TRUE
    in_mask = in_mask_vec
  }
  data$.stata_temp_mask = in_mask

  pipe_el = c("data")
  group_vars = expand_varlist(paste(group_vars, collapse=" "), names(data))
  if (length(group_vars) > 0) pipe_el = c(pipe_el, paste0("dplyr::group_by(!!!dplyr::syms(c('", paste(group_vars, collapse="','"), "')))"))
  pipe_el = c(pipe_el, paste0("dplyr::mutate(`", var_actual, "` = ", expr_body, ")"))
  if (length(group_vars) > 0) pipe_el = c(pipe_el, "dplyr::ungroup()")

  eval_env = s2r_stata_env(parent.frame())
  eval_env$data = data

  data = eval(parse(text = paste(pipe_el, collapse = " %>% ")), envir = eval_env)
  data$.stata_temp_mask = NULL

  return(data)
}

