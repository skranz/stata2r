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
t_egen = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_egen")
  parsed = s2r_p_egen(rest_of_cmd)
  if (is.na(parsed$new_var)) return(paste0("# Failed to parse egen command: ", rest_of_cmd))

  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, context)
  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  final_cond = NA_character_
  if (!is.na(r_if_cond)) final_cond = r_if_cond
  if (!is.na(r_in_range)) final_cond = if(is.na(final_cond)) r_in_range else paste0("(", final_cond, ") & (", r_in_range, ")")

  r_args = translate_stata_expression_with_r_values(parsed$args_str, line_num, cmd_df, context)
  r_args_cond = if (!is.na(final_cond)) paste0("dplyr::if_else(fast_coalesce(", final_cond, ", FALSE), ", r_args, ", NA)") else r_args

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
  else if (parsed$func_name %in% c("sd", "std")) calc_expr = paste0("stats::sd(", r_args_cond, ", na.rm = TRUE)")
  else if (parsed$func_name == "group") { needs_temp_sort = !cmd_obj$is_by_prefix; calc_expr = "dplyr::cur_group_id()" }
  else if (parsed$func_name == "tag") { needs_temp_sort = !cmd_obj$is_by_prefix; calc_expr = "as.numeric(dplyr::row_number() == 1)" }
  else if (parsed$func_name %in% c("rowtotal", "rowmean", "concat")) {
    is_row = TRUE
    calc_expr = paste0(".ROWOP_", parsed$func_name, "_PLACEHOLDER.")
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

  if (length(group_vars) > 0) args = c(args, paste0("group_vars = c('", paste(group_vars, collapse="','"), "')"))
  if (parsed$func_name %in% c("group", "tag", "rowtotal", "rowmean", "concat")) args = c(args, paste0("args_str = ", quote_for_r_literal(parsed$args_str)))
  args = c(args, paste0("needs_temp_sort = ", needs_temp_sort), paste0("is_row = ", is_row))

  return(paste0("data = scmd_egen(", paste(args, collapse = ", "), ")"))
}

# 3. Runtime Execution Phase
scmd_recode = function(data, varlist_str, rules_templates, gen_vars_str = NA_character_, r_if_cond = NA_character_, is_string = FALSE, labels_map = NULL) {
  restore.point("scmd_recode")

  vars_actual = expand_varlist(varlist_str, names(data))

  new_vars = vars_actual
  if (!is.na(gen_vars_str)) {
    new_vars = stringi::stri_split_regex(gen_vars_str, "\\s+")[[1]]
    new_vars = new_vars[new_vars != ""]
    if (length(new_vars) != length(vars_actual)) stop("scmd_recode: gen() requires same number of vars.")
  }

  r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))

  for (i in seq_along(vars_actual)) {
    old_var = vars_actual[i]
    new_var = new_vars[i]

    old_attrs = attributes(data[[old_var]])

    r_rules = gsub(".VAR.", paste0("`", old_var, "`"), rules_templates, fixed = TRUE)
    # Also resolve abbreviations in rules
    r_rules = vapply(r_rules, function(r) resolve_abbrevs_in_expr(r, names(data)), character(1))

    # STATA FALLBACK: If values do not match any conditions, they are left unchanged.
    fallback = if (is_string) paste0("as.character(`", old_var, "`)") else paste0("as.numeric(`", old_var, "`)")
    r_rules = c(r_rules, paste0("TRUE ~ ", fallback))

    case_when_expr = paste0("dplyr::case_when(\n    ", paste(r_rules, collapse = ",\n    "), "\n  )")

    if (!is.na(r_if_cond) && r_if_cond != "") {
      final_val_expr = paste0("dplyr::if_else((fast_coalesce(as.numeric(", r_if_cond, "), 0) != 0), ", case_when_expr, ", `", old_var, "`)")
    } else {
      final_val_expr = case_when_expr
    }

    if (is_string) {
      final_val_expr = paste0("as.character(", final_val_expr, ")")
    } else {
      final_val_expr = paste0("as.numeric(", final_val_expr, ")")
    }

    data = eval(parse(text = paste0("dplyr::mutate(data, `", new_var, "` = ", final_val_expr, ")")), envir = list(data = data), enclos = parent.frame())

    # Restore or Assign Labels
    if (!is.null(labels_map)) {
      data[[new_var]] = haven::labelled(data[[new_var]], labels = labels_map)
    } else if (old_var == new_var && !is_string) {
      # If replacing and no new labels are provided, keep original labels matching Stata
      if (!is.null(old_attrs$labels)) attr(data[[new_var]], "labels") = old_attrs$labels
      if (!is.null(old_attrs$label)) attr(data[[new_var]], "label") = old_attrs$label
      if (!is.null(old_attrs$class) && "haven_labelled" %in% old_attrs$class) {
        class(data[[new_var]]) = old_attrs$class
      }
    }
  }

  return(data)
}



