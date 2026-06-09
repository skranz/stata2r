# 1. Parsing Phase
s2r_p_egen = function(rest_of_cmd) {
  restore.point("s2r_p_egen")
  rest_no_type = stringi::stri_replace_first_regex(rest_of_cmd, "^\\s*(?:byte|int|long|float|double|str\\d+|strL)\\s+", "")
  parts_eq = stringi::stri_split_fixed(rest_no_type, "=", n=2)[[1]]
  if(length(parts_eq) != 2) return(list(new_var=NA_character_))

  new_var = stringi::stri_trim_both(parts_eq[1])
  right_part = stringi::stri_trim_both(parts_eq[2])

  parts_comma = stringi::stri_split_fixed(right_part, ",", n=2)[[1]]
  func_args_if_part = stringi::stri_trim_both(parts_comma[1])
  options_str = if(length(parts_comma)==2) stringi::stri_trim_both(parts_comma[2]) else NA_character_

  parts_paren = stringi::stri_split_fixed(func_args_if_part, "(", n=2)[[1]]
  if(length(parts_paren) != 2) return(list(new_var=NA_character_))

  func_name = stringi::stri_trim_both(parts_paren[1])
  args_if_part = stringi::stri_trim_both(stringi::stri_replace_last_fixed(parts_paren[2], ")", ""))

  parsed = s2r_parse_if_in(args_if_part)

  list(new_var = new_var, func_name = func_name, args_str = parsed$base_str,
       if_str = parsed$if_str, in_str = parsed$in_str, options = options_str)
}

# 2. Code Generation Phase
# 2. Code Generation Phase
# 2. Code Generation Phase
t_egen = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_egen")
  parsed = s2r_p_egen(rest_of_cmd)
  if (is.na(parsed$new_var)) return(paste0("# Failed to parse egen command: ", rest_of_cmd))

  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, context)
  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  # For egen, we cannot simply combine r_in_range directly with r_if_cond as text because r_in_range is an index vector (e.g. "1:5")
  # We will pass r_in_range as a separate argument to scmd_egen.

  r_args = translate_stata_expression_with_r_values(parsed$args_str, line_num, cmd_df, context)

  # Inside scmd_egen we will have a .stata_temp_mask that incorporates if and in conditions.
  r_args_cond = paste0("dplyr::if_else(.stata_temp_mask, ", r_args, ", NA)")

  is_ftm = fast_coalesce(stringi::stri_detect_fixed(parsed$options, "fieldstrustmissings"), FALSE)
  is_row = FALSE
  needs_temp_sort = FALSE

  calc_expr = ""
  if (parsed$func_name == "mean") calc_expr = paste0("mean(", r_args_cond, ", na.rm = TRUE)")
  else if (parsed$func_name %in% c("total", "sum")) calc_expr = paste0("collapse::fsum(", r_args_cond, ", na.rm = TRUE)")
  else if (parsed$func_name == "count") calc_expr = paste0("sum(!is.na(", r_args_cond, "))")
  else if (parsed$func_name == "min") calc_expr = paste0("collapse::fmin(", r_args_cond, ", na.rm = TRUE)")
  else if (parsed$func_name == "max") calc_expr = paste0("collapse::fmax(", r_args_cond, ", na.rm = TRUE)")
  else if (parsed$func_name == "rank") {
    needs_temp_sort = !cmd_obj$is_by_prefix
    if (is_ftm) val = paste0("as.numeric(dplyr::if_else(is.na(", r_args_cond, "), Inf, ", r_args_cond, "))") else val = r_args_cond
    calc_expr = paste0("as.numeric(base::rank(", val, ", ties.method = 'average', na.last = 'keep'))")
  }
  else if (parsed$func_name %in% c("median", "p50")) calc_expr = paste0("stats::median(", r_args_cond, ", na.rm = TRUE)")
  else if (parsed$func_name == "pctile") {
    p_val = 50
    if (!is.na(parsed$options)) {
      p_match = stringi::stri_match_first_regex(parsed$options, "\\bp\\s*\\(([^)]+)\\)")
      if (!is.na(p_match[1,1])) p_val = as.numeric(p_match[1,2])
    }
    prob = p_val / 100
    calc_expr = paste0("collapse::fquantile(", r_args_cond, ", probs = ", prob, ", na.rm = TRUE)")
  }
  else if (parsed$func_name %in% c("sd", "std")) calc_expr = paste0("stats::sd(", r_args_cond, ", na.rm = TRUE)")
  else if (parsed$func_name == "group") { needs_temp_sort = !cmd_obj$is_by_prefix; calc_expr = "dplyr::cur_group_id()" }
  else if (parsed$func_name == "tag") { needs_temp_sort = !cmd_obj$is_by_prefix; calc_expr = "as.numeric(dplyr::row_number() == 1)" }
  else if (parsed$func_name %in% c("rowtotal", "rsum", "rowmean", "rmax", "rowmax", "concat")) {
    is_row = TRUE
    calc_expr = paste0(".ROWOP_", parsed$func_name, "_PLACEHOLDER.")
  }
  else if (parsed$func_name == "seq") {
    needs_temp_sort = !cmd_obj$is_by_prefix
    from_val = 1
    to_val = NA_real_
    block_val = 1
    if (!is.na(parsed$options)) {
      from_match = stringi::stri_match_first_regex(parsed$options, "\\bfrom\\s*\\((\\d+)\\)")
      if (!is.na(from_match[1,1])) from_val = as.numeric(from_match[1,2])
      to_match = stringi::stri_match_first_regex(parsed$options, "\\bto\\s*\\((\\d+)\\)")
      if (!is.na(to_match[1,1])) to_val = as.numeric(to_match[1,2])
      block_match = stringi::stri_match_first_regex(parsed$options, "\\bblock\\s*\\((\\d+)\\)")
      if (!is.na(block_match[1,1])) block_val = as.numeric(block_match[1,2])
    }
    if (block_val == 1) {
      if (from_val == 1) {
        calc_expr = "dplyr::row_number()"
      } else {
        calc_expr = paste0("dplyr::row_number() + ", from_val - 1)
      }
    } else {
      calc_expr = paste0("floor((dplyr::row_number() - 1) / ", block_val, ") + ", from_val)
    }
    if (!is.na(to_val)) {
      calc_expr = paste0("floor((dplyr::row_number() - 1) / ", block_val, ") %% (", to_val - from_val + 1, ") + ", from_val)
    }
    calc_expr = paste0("dplyr::if_else(.stata_temp_mask, as.numeric(", calc_expr, "), NA_real_)")
  }
  else return(paste0("# Egen func '", parsed$func_name, "' not implemented."))

  group_vars = character(0)
  if (cmd_obj$is_by_prefix) group_vars = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
  else if (!is.na(parsed$options)) {
    by_opt = stringi::stri_match_first_regex(parsed$options, "\\bby\\s*\\(([^)]+)\\)")
    if (!is.na(by_opt[1,1])) group_vars = stringi::stri_split_regex(stringi::stri_trim_both(by_opt[1,2]), "\\s+")[[1]]
  }
  group_vars = group_vars[group_vars != "" & !is.na(group_vars)]

  args = c("data = data", paste0("new_var = ", quote_for_r_literal(parsed$new_var)),
           paste0("func_name = ", quote_for_r_literal(parsed$func_name)),
           paste0("calc_expr = ", quote_for_r_literal(calc_expr)))

  if (!is.na(r_if_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  if (!is.na(r_in_range)) args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))

  if (length(group_vars) > 0) args = c(args, paste0("group_vars = c('", paste(group_vars, collapse="','"), "')"))
  if (parsed$func_name %in% c("group", "tag", "rowtotal", "rsum", "rowmean", "rmax", "rowmax", "concat")) args = c(args, paste0("args_str = ", quote_for_r_literal(parsed$args_str)))
  args = c(args, paste0("needs_temp_sort = ", needs_temp_sort), paste0("is_row = ", is_row))

  return(paste0("data = scmd_egen(", paste(args, collapse = ", "), ")"))
}

scmd_egen = function(data, new_var, func_name, calc_expr, r_if_cond = NA_character_, r_in_range = NA_character_, group_vars = character(0), args_str = NA_character_, needs_temp_sort = FALSE, is_row = FALSE) {
  restore.point("scmd_egen")

  group_vars_actual = expand_varlist(paste(group_vars, collapse=" "), names(data))
  if (func_name %in% c("group", "tag")) {
    arg_vars = expand_varlist(args_str, names(data))
    group_vars_actual = unique(c(group_vars_actual, arg_vars))
  }

  r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))

  # Compute mask globally
  in_mask = rep(TRUE, nrow(data))
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask_vec = rep(FALSE, nrow(data))
    in_mask_vec[idx] = TRUE
    in_mask = in_mask_vec
  }

  if (!is.na(r_if_cond) && r_if_cond != "") {
    data$.stata_temp_mask = in_mask & (fast_coalesce(as.numeric(s2r_eval_cond(data, r_if_cond, envir = parent.frame())), 0) != 0)
  } else {
    data$.stata_temp_mask = in_mask
  }

  if (is_row) {
    row_vars = expand_varlist(args_str, names(data))
    if (func_name %in% c("rowtotal", "rsum")) {
      calc_expr = paste0("dplyr::if_else(.stata_temp_mask, base::rowSums(replace(dplyr::select(data, dplyr::all_of(c('", paste(row_vars, collapse="','"), "'))), is.na(dplyr::select(data, dplyr::all_of(c('", paste(row_vars, collapse="','"), "')))), 0), na.rm = FALSE), NA_real_)")
    } else if (func_name == "rowmean") {
      calc_expr = paste0("dplyr::if_else(.stata_temp_mask, base::rowMeans(dplyr::select(data, dplyr::all_of(c('", paste(row_vars, collapse="','"), "'))), na.rm = TRUE), NA_real_)")
    } else if (func_name %in% c("rmax", "rowmax")) {
      calc_expr = paste0("dplyr::if_else(.stata_temp_mask, suppressWarnings(do.call(pmax, c(unname(as.list(dplyr::select(data, dplyr::all_of(c('", paste(row_vars, collapse="','"), "'))))), list(na.rm = TRUE)))), NA_real_)")
    } else if (func_name == "concat") {
      na_checks = paste0("is.na(data[['", row_vars, "']])", collapse=" & ")
      stri_args = paste0("dplyr::if_else(is.na(as.character(data[['", row_vars, "']])), \"\", as.character(data[['", row_vars, "']]))", collapse=", ")
      calc_expr = paste0("dplyr::if_else(!.stata_temp_mask | (", na_checks, "), NA_character_, stringi::stri_paste(", stri_args, ", sep = ''))")
    }
  } else {
    calc_expr = resolve_abbrevs_in_expr(calc_expr, names(data))
  }

  eval_env = s2r_stata_env(parent.frame())

  if (needs_temp_sort) {
    tmp = data
    sort_vars = group_vars_actual
    if (func_name == "rank") sort_vars = unique(c(sort_vars, expand_varlist(args_str, names(data))))
    if ("stata2r_original_order_idx" %in% names(tmp)) sort_vars = c(sort_vars, "stata2r_original_order_idx")

    if (length(sort_vars) > 0) {
      sort_cmd = paste0("dplyr::arrange(tmp, ", paste(paste0("`", sort_vars, "`"), collapse=", "), ")")
      eval_env$tmp = tmp
      tmp = eval(parse(text = sort_cmd), envir = eval_env)
    }

    pipe_el = c("tmp")
    if (length(group_vars_actual) > 0) pipe_el = c(pipe_el, paste0("dplyr::group_by(!!!dplyr::syms(c('", paste(group_vars_actual, collapse="','"), "')))"))
    pipe_el = c(pipe_el, paste0("dplyr::mutate(`", new_var, "` = ", calc_expr, ")"))
    if (length(group_vars_actual) > 0) pipe_el = c(pipe_el, "dplyr::ungroup()")

    eval_env$tmp = tmp
    tmp = eval(parse(text = paste(pipe_el, collapse = " %>% ")), envir = eval_env)

    if ("stata2r_original_order_idx" %in% names(data)) {
      data = dplyr::left_join(data, tmp[, c("stata2r_original_order_idx", new_var)], by = "stata2r_original_order_idx")
    } else {
      data[[new_var]] = tmp[[new_var]]
    }

  } else {
    pipe_el = c("data")
    if (length(group_vars_actual) > 0 && !is_row) pipe_el = c(pipe_el, paste0("dplyr::group_by(!!!dplyr::syms(c('", paste(group_vars_actual, collapse="','"), "')))"))
    pipe_el = c(pipe_el, paste0("dplyr::mutate(`", new_var, "` = ", calc_expr, ")"))
    if (length(group_vars_actual) > 0 && !is_row) pipe_el = c(pipe_el, "dplyr::ungroup()")

    eval_env$data = data
    data = eval(parse(text = paste(pipe_el, collapse = " %>% ")), envir = eval_env)
  }

  # For rank, group, tag, where if_cond/in_range usually creates NA for unselected rows
  if (func_name %in% c("rank", "group", "tag")) {
     data[[new_var]] = dplyr::if_else(data$.stata_temp_mask, data[[new_var]], NA_real_)
  }

  data$.stata_temp_mask = NULL
  return(data)
}
