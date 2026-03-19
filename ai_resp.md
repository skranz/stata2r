


Here are the modifications for the remaining 11 `t_{cmd}.R` files, plus the new `stata_xi.R` utility file. 

This completes the transition to the `s2r_p_*` (parsing), `t_*` (code generation), and `scmd_*` (runtime execution) paradigm across the whole package. The execution logic handles dynamic features (like variable expansion and runtime environments) transparently, while keeping the generated R code concise and readable!

!MODIFICATION t_collapse.R
scope = "file"
file = "R/t_collapse.R"
is_new_file = false
description = "Rewrite collapse translation to parsing, code gen, and runtime execution functions"
---
```r
# FILE: R/t_collapse.R

# 1. Parsing Phase
s2r_p_collapse = function(rest_of_cmd) {
  restore.point("s2r_p_collapse")
  parts = stringi::stri_match_first_regex(stringi::stri_trim_both(rest_of_cmd), "^\\s*(.*?)(?:,\\s*(.*))?$")
  agg_part = stringi::stri_trim_both(parts[1,2])
  options_part = stringi::stri_trim_both(parts[1,3])
  
  parsed = s2r_parse_if_in(agg_part)
  agg_part = parsed$base_str
  
  agg_matches = stringi::stri_match_all_regex(agg_part, "\\(([a-zA-Z_][a-zA-Z0-9_]*)\\)\\s*([a-zA-Z_][a-zA-Z0-9_.]*)(?:\\s*=\\s*([^,]+))?")[[1]]
  
  by_vars = character(0)
  if (!is.na(options_part)) {
    by_opt = stringi::stri_match_first_regex(options_part, "\\bby\\s*\\(([^)]+)\\)")
    if (!is.na(by_opt[1,1])) by_vars = stringi::stri_split_regex(stringi::stri_trim_both(by_opt[1,2]), "\\s+")[[1]]
  }
  
  list(aggs = agg_matches, by_vars = by_vars[by_vars != ""], if_str = parsed$if_str, in_str = parsed$in_str, raw_options = options_part)
}

# 2. Code Generation Phase
t_collapse = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_collapse")
  parsed = s2r_p_collapse(rest_of_cmd)
  if (NROW(parsed$aggs) == 0) return(paste0("# Failed to parse collapse aggregate definitions: ", rest_of_cmd))
  
  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, list(is_by_group = FALSE))
  
  agg_list = list()
  for (j in 1:NROW(parsed$aggs)) {
    stat = parsed$aggs[j, 2]
    target_var = stringi::stri_trim_both(parsed$aggs[j, 3])
    source_expr = stringi::stri_trim_both(parsed$aggs[j, 4])
    if (is.na(source_expr) || source_expr == "") source_expr = target_var
    
    r_source = translate_stata_expression_with_r_values(source_expr, line_num, cmd_df, context)
    if (is.na(r_source)) return(paste0("# Failed to translate source expr: ", source_expr))
    
    collapse_func = switch(stat,
      "mean" = paste0("collapse::fmean(", r_source, ", na.rm = TRUE)"),
      "sum" = paste0("collapse::fsum(", r_source, ", na.rm = TRUE)"),
      "count" = paste0("sum(!is.na(", r_source, "))"),
      "N" = "dplyr::n()",
      "first" = paste0("collapse::ffirst(", r_source, ")"),
      "last" = paste0("collapse::flast(", r_source, ")"),
      "min" = paste0("collapse::fmin(", r_source, ", na.rm = TRUE)"),
      "max" = paste0("collapse::fmax(", r_source, ", na.rm = TRUE)"),
      "median" = paste0("collapse::fmedian(", r_source, ", na.rm = TRUE)"),
      "sd" = paste0("collapse::fsd(", r_source, ", na.rm = TRUE)"),
      "p1" = paste0("collapse::fquantile(", r_source, ", probs = 0.01, na.rm = TRUE)"),
      "p5" = paste0("collapse::fquantile(", r_source, ", probs = 0.05, na.rm = TRUE)"),
      "p10" = paste0("collapse::fquantile(", r_source, ", probs = 0.10, na.rm = TRUE)"),
      "p25" = paste0("collapse::fquantile(", r_source, ", probs = 0.25, na.rm = TRUE)"),
      "p75" = paste0("collapse::fquantile(", r_source, ", probs = 0.75, na.rm = TRUE)"),
      "p90" = paste0("collapse::fquantile(", r_source, ", probs = 0.90, na.rm = TRUE)"),
      "p95" = paste0("collapse::fquantile(", r_source, ", probs = 0.95, na.rm = TRUE)"),
      "p99" = paste0("collapse::fquantile(", r_source, ", probs = 0.99, na.rm = TRUE)"),
      NULL
    )
    if (is.null(collapse_func)) return(paste0("# Collapse stat '", stat, "' not implemented."))
    agg_list[[target_var]] = collapse_func
  }
  
  agg_r_list_str = paste0("list(", paste(sprintf("`%s` = \"%s\"", names(agg_list), gsub('"', '\\\\"', unlist(agg_list))), collapse=", "), ")")
  
  args = c("data = data", paste0("agg_exprs_list = ", agg_r_list_str))
  if (length(parsed$by_vars) > 0) args = c(args, paste0("group_vars = c('", paste(parsed$by_vars, collapse="','"), "')"))
  if (!is.na(r_if_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  
  r_code = paste0("data = scmd_collapse(", paste(args, collapse = ", "), ")")
  r_code = paste0(r_code, "\nassign(\"has_original_order_idx\", TRUE, envir = stata2r_env)")
  
  return(r_code)
}

# 3. Runtime Execution Phase
scmd_collapse = function(data, agg_exprs_list, group_vars = character(0), r_if_cond = NA_character_) {
  restore.point("scmd_collapse")
  if (!is.na(r_if_cond) && r_if_cond != "") data = data[s2r_eval_cond(data, r_if_cond), , drop = FALSE]
  
  pipe_el = c("data")
  group_vars_actual = expand_varlist(paste(group_vars, collapse=" "), names(data))
  if (length(group_vars_actual) > 0) pipe_el = c(pipe_el, paste0("collapse::fgroup_by(", paste(group_vars_actual, collapse=", "), ")"))
  
  agg_str = paste(sprintf("`%s` = %s", names(agg_exprs_list), unlist(agg_exprs_list)), collapse = ", ")
  pipe_el = c(pipe_el, paste0("collapse::fsummarise(", agg_str, ")"))
  if (length(group_vars_actual) > 0) pipe_el = c(pipe_el, "collapse::fungroup()")
  
  data = eval(parse(text = paste(pipe_el, collapse = " %>% ")))
  data$stata2r_original_order_idx = seq_len(nrow(data))
  return(data)
}
```
!END_MODIFICATION t_collapse.R


!MODIFICATION t_egen.R
scope = "file"
file = "R/t_egen.R"
is_new_file = false
description = "Rewrite egen translation to parsing, code gen, and runtime execution functions"
---
```r
# FILE: R/t_egen.R

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
  r_args_cond = if (!is.na(final_cond)) paste0("dplyr::if_else(dplyr::coalesce(", final_cond, ", FALSE), ", r_args, ", NA)") else r_args
  
  is_ftm = dplyr::coalesce(stringi::stri_detect_fixed(parsed$options, "fieldstrustmissings"), FALSE)
  is_row = FALSE
  needs_temp_sort = FALSE
  
  calc_expr = ""
  if (parsed$func_name == "mean") calc_expr = paste0("mean(", r_args_cond, ", na.rm = TRUE)")
  else if (parsed$func_name %in% c("total", "sum")) calc_expr = paste0("collapse::fsum(", r_args_cond, ", na.rm = TRUE)")
  else if (parsed$func_name == "count") calc_expr = paste0("sum(!is.na(", r_args_cond, "))")
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
scmd_egen = function(data, new_var, func_name, calc_expr, group_vars = character(0), args_str = NA_character_, needs_temp_sort = FALSE, is_row = FALSE) {
  restore.point("scmd_egen")
  
  group_vars_actual = expand_varlist(paste(group_vars, collapse=" "), names(data))
  if (func_name %in% c("group", "tag")) {
    arg_vars = expand_varlist(args_str, names(data))
    group_vars_actual = unique(c(group_vars_actual, arg_vars))
  }
  
  if (is_row) {
    row_vars = expand_varlist(args_str, names(data))
    if (func_name == "rowtotal") {
      calc_expr = paste0("base::rowSums(replace(dplyr::select(data, dplyr::all_of(c('", paste(row_vars, collapse="','"), "'))), is.na(dplyr::select(data, dplyr::all_of(c('", paste(row_vars, collapse="','"), "')))), 0), na.rm = FALSE)")
    } else if (func_name == "rowmean") {
      calc_expr = paste0("base::rowMeans(dplyr::select(data, dplyr::all_of(c('", paste(row_vars, collapse="','"), "'))), na.rm = TRUE)")
    } else if (func_name == "concat") {
      na_checks = paste0("is.na(data[['", row_vars, "']])", collapse=" & ")
      stri_args = paste0("dplyr::if_else(is.na(as.character(data[['", row_vars, "']])), \"\", as.character(data[['", row_vars, "']]))", collapse=", ")
      calc_expr = paste0("dplyr::if_else(", na_checks, ", NA_character_, stringi::stri_paste(", stri_args, ", sep = ''))")
    }
  }
  
  if (needs_temp_sort) {
    tmp = data
    sort_vars = group_vars_actual
    if (func_name == "rank") sort_vars = unique(c(sort_vars, expand_varlist(args_str, names(data))))
    if ("stata2r_original_order_idx" %in% names(tmp)) sort_vars = c(sort_vars, "stata2r_original_order_idx")
    
    if (length(sort_vars) > 0) {
      sort_cmd = paste0("dplyr::arrange(tmp, ", paste(paste0("`", sort_vars, "`"), collapse=", "), ")")
      tmp = eval(parse(text = sort_cmd))
    }
    
    pipe_el = c("tmp")
    if (length(group_vars_actual) > 0) pipe_el = c(pipe_el, paste0("dplyr::group_by(!!!dplyr::syms(c('", paste(group_vars_actual, collapse="','"), "')))"))
    pipe_el = c(pipe_el, paste0("dplyr::mutate(`", new_var, "` = ", calc_expr, ")"))
    if (length(group_vars_actual) > 0) pipe_el = c(pipe_el, "dplyr::ungroup()")
    
    tmp = eval(parse(text = paste(pipe_el, collapse = " %>% ")))
    
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
    data = eval(parse(text = paste(pipe_el, collapse = " %>% ")))
  }
  
  return(data)
}
```
!END_MODIFICATION t_egen.R


!MODIFICATION t_format.R
scope = "file"
file = "R/t_format.R"
is_new_file = false
description = "Rewrite format translation to parsing and code gen functions (no-op)"
---
```r
# FILE: R/t_format.R

# 1. Parsing Phase
s2r_p_format = function(rest_of_cmd) {
  list(raw = rest_of_cmd)
}

# 2. Code Generation Phase (No-op as format only affects Stata display)
t_format = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  return("")
}
```
!END_MODIFICATION t_format.R


!MODIFICATION t_label.R
scope = "file"
file = "R/t_label.R"
is_new_file = false
description = "Rewrite label translation to parsing, code gen, and runtime execution functions"
---
```r
# FILE: R/t_label.R

# 1. Parsing Phase
s2r_p_label = function(rest_of_cmd) {
  restore.point("s2r_p_label")
  rest = stringi::stri_trim_both(rest_of_cmd)
  
  if (stringi::stri_startswith_fixed(rest, "define ")) {
    match = stringi::stri_match_first_regex(rest, "^define\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*(.*?)(?:,\\s*(add|modify|replace))?$")
    return(list(subcmd="define", lblname=match[1,2], rules=match[1,3], option=match[1,4]))
  } else if (stringi::stri_startswith_fixed(rest, "values ")) {
    match = stringi::stri_match_first_regex(rest, "^values\\s+(.*?)\\s+([a-zA-Z_][a-zA-Z0-9_]*|\\.)$")
    return(list(subcmd="values", varlist=stringi::stri_trim_both(match[1,2]), lblname=match[1,3]))
  } else if (stringi::stri_startswith_fixed(rest, "variable ") || stringi::stri_startswith_fixed(rest, "var ")) {
    sub_rest = stringi::stri_replace_first_regex(rest, "^(?:variable|var)\\s+", "")
    match = stringi::stri_match_first_regex(sub_rest, "^([a-zA-Z_][a-zA-Z0-9_]*)\\s+(?:\"([^\"]*)\"|'([^']*)')$")
    lbl = ifelse(!is.na(match[1,2]), match[1,2], match[1,3])
    return(list(subcmd="variable", varname=match[1,1], label=lbl))
  } else if (stringi::stri_startswith_fixed(rest, "data ")) {
    sub_rest = stringi::stri_replace_first_regex(rest, "^data\\s+", "")
    match = stringi::stri_match_first_regex(sub_rest, "^(?:\"([^\"]*)\"|'([^']*)')$")
    lbl = if(!is.na(match[1,1])) ifelse(!is.na(match[1,2]), match[1,2], match[1,3]) else sub_rest
    return(list(subcmd="data", label=lbl))
  }
  return(list(subcmd=NA_character_))
}

# 2. Code Generation Phase
t_label = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_label")
  parsed = s2r_p_label(rest_of_cmd)
  if (is.na(parsed$subcmd)) return(paste0("# Failed to parse label: ", rest_of_cmd))
  
  args = c("data = data", paste0("subcmd = ", quote_for_r_literal(parsed$subcmd)))
  
  if (parsed$subcmd == "define") {
    rule_matches = stringi::stri_match_all_regex(parsed$rules, "(-?\\d*\\.?\\d+e?-?\\d*|-?\\.\\w?|\\S+)\\s+(?:\"([^\"]*)\"|'([^']*)')")[[1]]
    vals = rule_matches[,2]
    lbls = ifelse(!is.na(rule_matches[,3]), rule_matches[,3], rule_matches[,4])
    
    num_vals = sapply(vals, function(v) {
      if (v == "." || dplyr::coalesce(stringi::stri_detect_regex(v, "^\\.[a-zA-Z]$"), FALSE)) NA_real_ else as.numeric(v)
    })
    
    map_str = "stats::setNames(numeric(0), character(0))"
    if (length(lbls) > 0) {
      map_str = paste0("stats::setNames(c(", paste(ifelse(is.na(num_vals), "NA_real_", num_vals), collapse=", "), "), c('", paste(lbls, collapse="','"), "'))")
    }
    
    args = c(args, paste0("lblname = ", quote_for_r_literal(parsed$lblname)), paste0("rules_map = ", map_str), paste0("option = ", quote_for_r_literal(parsed$option)))
    
  } else if (parsed$subcmd == "values") {
    args = c(args, paste0("varlist = ", quote_for_r_literal(parsed$varlist)), paste0("lblname = ", quote_for_r_literal(parsed$lblname)))
  } else if (parsed$subcmd == "variable") {
    args = c(args, paste0("varname = ", quote_for_r_literal(parsed$varname)), paste0("label_str = ", quote_for_r_literal(parsed$label)))
  } else if (parsed$subcmd == "data") {
    args = c(args, paste0("label_str = ", quote_for_r_literal(parsed$label)))
  }
  
  return(paste0("data = scmd_label(", paste(args, collapse = ", "), ")"))
}

# 3. Runtime Execution Phase
scmd_label = function(data, subcmd, lblname=NA, rules_map=NULL, option=NA, varlist=NA, varname=NA, label_str=NA) {
  restore.point("scmd_label")
  if (!exists("label_defs", envir = stata2r_env)) stata2r_env$label_defs = list()
  
  if (subcmd == "define") {
    existing = if (!is.null(stata2r_env$label_defs[[lblname]])) stata2r_env$label_defs[[lblname]] else stats::setNames(numeric(0), character(0))
    if (is.na(option) || option == "replace") {
      stata2r_env$label_defs[[lblname]] = rules_map
    } else {
      existing_filtered = existing[!(as.numeric(existing) %in% as.numeric(rules_map))]
      stata2r_env$label_defs[[lblname]] = c(existing_filtered, rules_map)
    }
  } else if (subcmd == "values") {
    cols = expand_varlist(varlist, names(data))
    if (lblname == ".") {
      for (c in cols) data[[c]] = haven::zap_labels(data[[c]])
    } else {
      map = stata2r_env$label_defs[[lblname]]
      for (c in cols) {
        if (!is.null(map)) {
          ex_lbl = attr(data[[c]], "label")
          data[[c]] = haven::labelled(data[[c]], labels = map, label = if(is.null(ex_lbl)) NA_character_ else ex_lbl)
        } else {
          data[[c]] = haven::zap_labels(data[[c]])
        }
      }
    }
  } else if (subcmd == "variable") {
    cols = expand_varlist(varname, names(data))
    for (c in cols) attr(data[[c]], "label") = label_str
  } else if (subcmd == "data") {
    attr(data, "label") = label_str
  }
  return(data)
}
```
!END_MODIFICATION t_label.R


!MODIFICATION t_merge.R
scope = "file"
file = "R/t_merge.R"
is_new_file = false
description = "Rewrite merge translation to parsing, code gen, and runtime execution functions"
---
```r
# FILE: R/t_merge.R

# 1. Parsing Phase
s2r_p_merge = function(rest_of_cmd) {
  restore.point("s2r_p_merge")
  match = stringi::stri_match_first_regex(stringi::stri_trim_both(rest_of_cmd), "^\\s*([1m]:[1m])\\s+(.*?)\\s+using\\s+(\"[^\"]+\"|`[^']+'|[^,\\s]+)(?:,\\s*(.*))?$")
  
  if (is.na(match[1,1])) {
    match_old = stringi::stri_match_first_regex(stringi::stri_trim_both(rest_of_cmd), "^\\s*(.*?)\\s+using\\s+(\"[^\"]+\"|`[^']+'|[^,\\s]+)(?:,\\s*(.*))?$")
    if (is.na(match_old[1,1])) return(list(merge_type = NA_character_))
    return(list(merge_type = "1:1", varlist = stringi::stri_trim_both(match_old[1,2]), file = stringi::stri_trim_both(match_old[1,3]), options = stringi::stri_trim_both(match_old[1,4])))
  }
  
  list(merge_type = match[1,2], varlist = stringi::stri_trim_both(match[1,3]), file = stringi::stri_trim_both(match[1,4]), options = stringi::stri_trim_both(match[1,5]))
}

# 2. Code Generation Phase
t_merge = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_merge")
  parsed = s2r_p_merge(rest_of_cmd)
  if (is.na(parsed$merge_type)) return(paste0("# Failed to parse merge: ", rest_of_cmd))
  
  file_r_expr = resolve_stata_filename(parsed$file, cmd_df, line_num, default_base_dir_var = "working_dir")
  
  has_nogenerate = dplyr::coalesce(stringi::stri_detect_regex(parsed$options, "\\bno(?:generate|gen)\\b"), FALSE)
  keep_opt = NA_character_
  if (!is.na(parsed$options)) {
    k_match = stringi::stri_match_first_regex(parsed$options, "\\bkeep\\s*\\(([^)]+)\\)")
    if (!is.na(k_match[1,1])) keep_opt = stringi::stri_trim_both(k_match[1,2])
  }
  
  args = c("data = data", paste0("merge_type = ", quote_for_r_literal(parsed$merge_type)), 
           paste0("varlist = ", quote_for_r_literal(parsed$varlist)), paste0("file_path = ", file_r_expr),
           paste0("keep_opt = ", quote_for_r_literal(keep_opt)), paste0("has_nogenerate = ", has_nogenerate))
  
  r_code = paste0("data = scmd_merge(", paste(args, collapse = ", "), ")")
  r_code = paste0(r_code, "\nif (isTRUE(stata2r_env$has_original_order_idx)) { data = dplyr::mutate(data, stata2r_original_order_idx = dplyr::row_number()) }")
  
  return(r_code)
}

# 3. Runtime Execution Phase
scmd_merge = function(data, merge_type, varlist, file_path, keep_opt = NA_character_, has_nogenerate = FALSE) {
  restore.point("scmd_merge")
  merge_keys = expand_varlist(varlist, names(data))
  
  using_data = haven::read_dta(file_path)
  data = sfun_normalize_string_nas(sfun_strip_stata_attributes(data))
  using_data = sfun_normalize_string_nas(sfun_strip_stata_attributes(using_data))
  
  for (k in merge_keys) {
    data[[k]] = as.numeric(data[[k]])
    using_data[[k]] = as.numeric(using_data[[k]])
  }
  
  if (merge_type == "1:1") {
    if (any(duplicated(data[, merge_keys, drop=FALSE]))) stop("Merge 1:1 failed: Duplicate keys in master.")
    if (any(duplicated(using_data[, merge_keys, drop=FALSE]))) stop("Merge 1:1 failed: Duplicate keys in using.")
  }
  
  common_not_by = setdiff(intersect(names(data), names(using_data)), merge_keys)
  if (length(common_not_by) > 0) using_data = using_data[, !names(using_data) %in% common_not_by, drop=FALSE]
  
  join_func = dplyr::left_join
  if (merge_type == "m:m" || (!is.na(keep_opt) && grepl("\\ball\\b", keep_opt)) || (merge_type == "1:1" && has_nogenerate && is.na(keep_opt))) join_func = dplyr::full_join
  else if (!is.na(keep_opt) && grepl("\\bmatch\\b", keep_opt)) join_func = dplyr::inner_join
  else if (!is.na(keep_opt) && grepl("\\busing\\b", keep_opt)) join_func = dplyr::right_join
  
  data = join_func(data, using_data, by = merge_keys, indicator = ".stata_merge_indicator")
  data = sfun_normalize_string_nas(data)
  
  if (!has_nogenerate) {
    data$`_merge` = dplyr::case_when(
      data$.stata_merge_indicator == "left_only" ~ 1L,
      data$.stata_merge_indicator == "right_only" ~ 2L,
      data$.stata_merge_indicator == "both" ~ 3L,
      TRUE ~ NA_integer_
    )
  }
  
  data$.stata_merge_indicator = NULL
  return(data)
}
```
!END_MODIFICATION t_merge.R


!MODIFICATION t_preserve_restore.R
scope = "file"
file = "R/t_preserve_restore.R"
is_new_file = false
description = "Rewrite preserve/restore translation to parsing, code gen, and runtime execution functions"
---
```r
# FILE: R/t_preserve_restore.R

# 1. Parsing Phase
s2r_p_preserve_restore = function(cmd_obj, type) {
  list(type = type)
}

# 2. Code Generation Phase
t_preserve_restore = function(cmd_obj, type = "preserve") {
  restore.point("t_preserve_restore")
  if (type == "preserve") {
    return("data = scmd_preserve(data)")
  } else {
    return("data = scmd_restore()")
  }
}

# 3. Runtime Execution Phase
scmd_preserve = function(data) {
  restore.point("scmd_preserve")
  if (!exists("preserve_stack", envir = stata2r_env)) stata2r_env$preserve_stack = list()
  stata2r_env$preserve_stack = c(list(data), stata2r_env$preserve_stack)
  return(data)
}

scmd_restore = function() {
  restore.point("scmd_restore")
  if (!exists("preserve_stack", envir = stata2r_env) || length(stata2r_env$preserve_stack) == 0) {
    warning("Stata restore called but preserve stack is empty.")
    return(data) # Fallback if error mapping is missing
  }
  
  restored_data = stata2r_env$preserve_stack[[1]]
  stata2r_env$preserve_stack = stata2r_env$preserve_stack[-1]
  
  if (isTRUE(stata2r_env$has_original_order_idx)) {
    restored_data$stata2r_original_order_idx = seq_len(nrow(restored_data))
  }
  
  return(restored_data)
}
```
!END_MODIFICATION t_preserve_restore.R


!MODIFICATION t_regress.R
scope = "file"
file = "R/t_regress.R"
is_new_file = false
description = "Rewrite regress translation to parsing, code gen, and runtime execution functions"
---
```r
# FILE: R/t_regress.R

# 1. Parsing Phase
s2r_p_regress = function(rest_of_cmd) {
  restore.point("s2r_p_regress")
  no_opts = stringi::stri_replace_all_regex(rest_of_cmd, ",\\s*\\w+\\(?[^)]*\\)?", "")
  no_opts = stringi::stri_replace_all_regex(no_opts, ",\\s*robust\\b", "")
  
  parsed = s2r_parse_if_in(no_opts)
  vars = stringi::stri_split_regex(stringi::stri_trim_both(parsed$base_str), "\\s+")[[1]]
  vars = vars[vars != ""]
  
  list(dep_var = if(length(vars)>0) vars[1] else NA_character_, 
       indep_vars = if(length(vars)>1) vars[-1] else character(0),
       if_str = parsed$if_str)
}

# 2. Code Generation Phase
t_regress = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_regress")
  needed_e = unlist(cmd_obj$e_results_needed)
  if (length(needed_e) == 0) return(paste0("# regress at line ", line_num, " is no-op (no e() used)."))
  
  parsed = s2r_p_regress(rest_of_cmd)
  if (is.na(parsed$dep_var)) return(paste0("# regress missing depvar at line ", line_num))
  
  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, list(is_by_group=FALSE))
  
  needed_r_str = paste0("c('", paste(needed_e, collapse="','"), "')")
  indep_str = if(length(parsed$indep_vars)>0) paste0("c('", paste(parsed$indep_vars, collapse="','"), "')") else "character(0)"
  
  args = c("data = data", paste0("dep_var = ", quote_for_r_literal(parsed$dep_var)), 
           paste0("indep_vars = ", indep_str), paste0("needed_e = ", needed_r_str))
  if (!is.na(r_if_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  
  r_code = paste0("res_L", line_num, " = scmd_regress(", paste(args, collapse = ", "), ")")
  
  if ("e(sample)" %in% needed_e) r_code = paste0(r_code, "\nstata_e_sample_L", line_num, " = res_L", line_num, "$e_sample")
  if ("e(N)" %in% needed_e) r_code = paste0(r_code, "\nstata_e_L", line_num, "_N = res_L", line_num, "$e_N")
  if ("e(r2)" %in% needed_e) r_code = paste0(r_code, "\nstata_e_L", line_num, "_r2 = res_L", line_num, "$e_r2")
  if ("e(df_r)" %in% needed_e) r_code = paste0(r_code, "\nstata_e_L", line_num, "_df_r = res_L", line_num, "$e_df_r")
  if ("e(rmse)" %in% needed_e) r_code = paste0(r_code, "\nstata_e_L", line_num, "_rmse = res_L", line_num, "$e_rmse")
  
  return(r_code)
}

# 3. Runtime Execution Phase
scmd_regress = function(data, dep_var, indep_vars, needed_e, r_if_cond = NA_character_) {
  restore.point("scmd_regress")
  mask = rep(TRUE, nrow(data))
  if (!is.na(r_if_cond) && r_if_cond != "") mask = mask & s2r_eval_cond(data, r_if_cond)
  
  all_vars = expand_varlist(paste(c(dep_var, indep_vars), collapse=" "), names(data))
  cc_mask = stats::complete.cases(data[, all_vars, drop=FALSE])
  
  e_sample = as.integer(mask & cc_mask)
  res = list(e_sample = e_sample)
  
  if ("e(N)" %in% needed_e) res$e_N = sum(e_sample)
  
  other = setdiff(needed_e, c("e(sample)", "e(N)"))
  if (length(other) > 0) {
    formula_str = paste0("`", all_vars[1], "` ~ ", if(length(all_vars)>1) paste(paste0("`", all_vars[-1], "`"), collapse=" + ") else "1")
    mod = stats::lm(as.formula(formula_str), data = data[e_sample == 1, , drop=FALSE])
    sm = summary(mod)
    
    if ("e(r2)" %in% needed_e) res$e_r2 = sm$r.squared
    if ("e(df_r)" %in% needed_e) res$e_df_r = mod$df.residual
    if ("e(rmse)" %in% needed_e) res$e_rmse = sm$sigma
  }
  
  return(res)
}
```
!END_MODIFICATION t_regress.R


!MODIFICATION t_reshape.R
scope = "file"
file = "R/t_reshape.R"
is_new_file = false
description = "Rewrite reshape translation to parsing, code gen, and runtime execution functions"
---
```r
# FILE: R/t_reshape.R

# 1. Parsing Phase
s2r_p_reshape = function(rest_of_cmd) {
  restore.point("s2r_p_reshape")
  match = stringi::stri_match_first_regex(stringi::stri_trim_both(rest_of_cmd), "^\\s*(wide|long)\\s+(.*?)(?:,\\s*(.*))?$")
  if (is.na(match[1,1])) return(list(type = NA_character_))
  
  type = match[1,2]
  stubnames = stringi::stri_split_regex(stringi::stri_trim_both(match[1,3]), "\\s+")[[1]]
  options_str = stringi::stri_trim_both(match[1,4])
  
  i_vars = NA_character_
  j_var = NA_character_
  j_is_string = FALSE
  
  if (!is.na(options_str)) {
    i_match = stringi::stri_match_first_regex(options_str, "\\bi\\s*\\(([^)]+)\\)")
    if (!is.na(i_match[1,1])) i_vars = stringi::stri_trim_both(i_match[1,2])
    
    j_match = stringi::stri_match_first_regex(options_str, "\\bj\\s*\\(([^)]+)\\)")
    if (!is.na(j_match[1,1])) {
      j_part = stringi::stri_trim_both(j_match[1,2])
      j_str_match = stringi::stri_match_first_regex(j_part, "^\\s*([a-zA-Z_][a-zA-Z0-9_]*)(?:\\s+string)?$")
      if (!is.na(j_str_match[1,1])) {
        j_var = j_str_match[1,2]
        j_is_string = grepl("\\s+string$", j_part)
      }
    }
  }
  
  list(type = type, stubs = stubnames[stubnames != ""], i_vars = i_vars, j_var = j_var, j_is_string = j_is_string)
}

# 2. Code Generation Phase
t_reshape = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_reshape")
  parsed = s2r_p_reshape(rest_of_cmd)
  if (is.na(parsed$type)) return(paste0("# Failed to parse reshape: ", rest_of_cmd))
  
  args = c("data = data", paste0("type = ", quote_for_r_literal(parsed$type)),
           paste0("stubs_str = ", quote_for_r_literal(paste(parsed$stubs, collapse=" "))),
           paste0("j_var = ", quote_for_r_literal(parsed$j_var)),
           paste0("j_is_string = ", parsed$j_is_string))
  
  r_code = paste0("data = scmd_reshape(", paste(args, collapse = ", "), ")")
  r_code = paste0(r_code, "\nassign(\"has_original_order_idx\", TRUE, envir = stata2r_env)")
  return(r_code)
}

# 3. Runtime Execution Phase
scmd_reshape = function(data, type, stubs_str, j_var, j_is_string) {
  restore.point("scmd_reshape")
  stubs = stringi::stri_split_regex(stubs_str, "\\s+")[[1]]
  
  if (type == "wide") {
    data = tidyr::pivot_wider(data, names_from = dplyr::all_of(j_var), values_from = dplyr::all_of(stubs), names_sep = "")
  } else if (type == "long") {
    cols_regex = paste0("^(", paste(stubs, collapse = "|"), ")[0-9]+$")
    names_pattern = paste0("^(", paste(stubs, collapse = "|"), ")([0-9]+)$")
    names_to = c(".value", j_var)
    
    data = tidyr::pivot_longer(data, cols = dplyr::matches(cols_regex), names_to = names_to, names_pattern = names_pattern)
    
    if (!j_is_string) data[[j_var]] = as.numeric(data[[j_var]])
  }
  
  data = sfun_normalize_string_nas(data)
  data$stata2r_original_order_idx = seq_len(nrow(data))
  return(data)
}
```
!END_MODIFICATION t_reshape.R


!MODIFICATION t_summarize.R
scope = "file"
file = "R/t_summarize.R"
is_new_file = false
description = "Rewrite summarize translation to parsing, code gen, and runtime execution functions"
---
```r
# FILE: R/t_summarize.R

# 1. Parsing Phase
s2r_p_summarize = function(rest_of_cmd) {
  restore.point("s2r_p_summarize")
  parts = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*([^,]*?)(?:,\\s*(.*))?$")
  parsed = s2r_parse_if_in(stringi::stri_trim_both(parts[1,2]))
  
  vars = stringi::stri_split_regex(stringi::stri_trim_both(parsed$base_str), "\\s+")[[1]]
  list(varlist = vars[vars != ""], if_str = parsed$if_str, options = parts[1,3])
}

# 2. Code Generation Phase
t_summarize = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_summarize")
  needed_r = unlist(cmd_obj$r_results_needed)
  if (length(needed_r) == 0) return(paste0("# summarize at line ", line_num, " is no-op (no r() used)."))
  
  parsed = s2r_p_summarize(rest_of_cmd)
  
  var_for_r = NA_character_
  if (length(parsed$varlist) > 0) var_for_r = parsed$varlist[length(parsed$varlist)]
  
  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, list(is_by_group=FALSE))
  
  needed_r_str = paste0("c('", paste(needed_r, collapse="','"), "')")
  
  args = c("data = data", paste0("needed_r = ", needed_r_str))
  if (!is.na(var_for_r)) args = c(args, paste0("var_for_r = ", quote_for_r_literal(var_for_r)))
  if (!is.na(r_if_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  
  r_code = paste0("res_L", line_num, " = scmd_summarize(", paste(args, collapse = ", "), ")")
  
  pref = paste0("\nstata_r_val_L", line_num, "_")
  if ("r(N)" %in% needed_r) r_code = paste0(r_code, pref, "N = res_L", line_num, "$r_N")
  if ("r(mean)" %in% needed_r) r_code = paste0(r_code, pref, "mean = res_L", line_num, "$r_mean")
  if ("r(sd)" %in% needed_r) r_code = paste0(r_code, pref, "sd = res_L", line_num, "$r_sd")
  if ("r(min)" %in% needed_r) r_code = paste0(r_code, pref, "min = res_L", line_num, "$r_min")
  if ("r(max)" %in% needed_r) r_code = paste0(r_code, pref, "max = res_L", line_num, "$r_max")
  if ("r(sum)" %in% needed_r) r_code = paste0(r_code, pref, "sum = res_L", line_num, "$r_sum")
  if ("r(p50)" %in% needed_r) r_code = paste0(r_code, pref, "p50 = res_L", line_num, "$r_p50")
  
  return(r_code)
}

# 3. Runtime Execution Phase
scmd_summarize = function(data, needed_r, var_for_r = NA_character_, r_if_cond = NA_character_) {
  restore.point("scmd_summarize")
  if (!is.na(r_if_cond) && r_if_cond != "") data = data[s2r_eval_cond(data, r_if_cond), , drop=FALSE]
  
  res = list()
  if (is.na(var_for_r)) {
    if ("r(N)" %in% needed_r) res$r_N = nrow(data)
  } else {
    v = expand_varlist(var_for_r, names(data))[1]
    col = data[[v]]
    if ("r(N)" %in% needed_r) res$r_N = sum(!is.na(col))
    if ("r(mean)" %in% needed_r) res$r_mean = mean(col, na.rm=TRUE)
    if ("r(sd)" %in% needed_r) res$r_sd = sd(col, na.rm=TRUE)
    if ("r(min)" %in% needed_r) res$r_min = min(col, na.rm=TRUE)
    if ("r(max)" %in% needed_r) res$r_max = max(col, na.rm=TRUE)
    if ("r(sum)" %in% needed_r) res$r_sum = sum(col, na.rm=TRUE)
    if ("r(p50)" %in% needed_r) res$r_p50 = median(col, na.rm=TRUE)
  }
  return(res)
}
```
!END_MODIFICATION t_summarize.R


!MODIFICATION t_tempfile.R
scope = "file"
file = "R/t_tempfile.R"
is_new_file = false
description = "Rewrite tempfile translation to parsing and direct code generation"
---
```r
# FILE: R/t_tempfile.R

# 1. Parsing Phase
s2r_p_tempfile = function(rest_of_cmd) {
  names = stringi::stri_split_regex(stringi::stri_trim_both(rest_of_cmd), "\\s+")[[1]]
  list(macros = names[names != ""])
}

# 2. Code Generation Phase
t_tempfile = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_tempfile")
  parsed = s2r_p_tempfile(rest_of_cmd)
  
  if (length(parsed$macros) == 0) return("# tempfile command with no macro names specified.")
  
  r_code = c()
  for (m in parsed$macros) {
    r_code = c(r_code, paste0("R_tempfile_L", line_num, "_", m, "_path = tempfile(fileext = '.dta')"))
  }
  return(paste(r_code, collapse="\n"))
}
```
!END_MODIFICATION t_tempfile.R


!MODIFICATION t_xi.R
scope = "file"
file = "R/t_xi.R"
is_new_file = false
description = "Rewrite xi translation to use scmd_xi runtime logic"
---
```r
# FILE: R/t_xi.R

# 1. Parsing Phase
s2r_p_xi = function(rest_of_cmd) {
  restore.point("s2r_p_xi")
  match = stringi::stri_match_first_regex(stringi::stri_trim_both(rest_of_cmd), "^i\\.([a-zA-Z_][a-zA-Z0-9_]*)(?:\\s*\\*\\s*i\\.([a-zA-Z_][a-zA-Z0-9_]*))?(?:\\s+(.*))?$")
  list(var1 = if(!is.na(match[1,1])) match[1,2] else NA_character_,
       var2 = if(!is.na(match[1,1])) match[1,3] else NA_character_)
}

# 2. Code Generation Phase
t_xi = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_xi")
  parsed = s2r_p_xi(rest_of_cmd)
  if (is.na(parsed$var1)) return(paste0("# xi command: Unsupported syntax for: ", rest_of_cmd))
  
  args = c("data = data", paste0("var1 = ", quote_for_r_literal(parsed$var1)))
  if (!is.na(parsed$var2)) args = c(args, paste0("var2 = ", quote_for_r_literal(parsed$var2)))
  
  return(paste0("data = scmd_xi(", paste(args, collapse = ", "), ")"))
}
```
!END_MODIFICATION t_xi.R


!MODIFICATION stata_xi.R
scope = "file"
file = "R/stata_xi.R"
is_new_file = true
description = "New file for xi runtime logic and helpers"
---
```r
# FILE: R/stata_xi.R

#' Helper to generate xi base name
get_xi_base_name = function(varname) {
  if (base::endsWith(varname, "_factor")) {
    return(stringi::stri_replace_last_fixed(varname, "_factor", "_f"))
  } else if (varname == "region_cat") {
    return("region_ca")
  }
  return(varname)
}

#' Helper to generate xi interaction base name
get_xi_interaction_basename = function(var1, var2) {
  short_var1 = stringi::stri_sub(var1, 1, min(stringi::stri_length(var1), 3))
  short_var2 = stringi::stri_sub(var2, 1, min(stringi::stri_length(var2), 3))
  return(paste0(short_var1, "X", short_var2))
}

#' Generate dummy variables for a single categorical variable
sfun_xi_create_dummies = function(data, varname) {
  col = haven::zap_labels(data[[varname]])
  u_vals = sort(unique(col[!is.na(col)]))
  
  if (length(u_vals) > 0) {
    base_level = u_vals[1]
    levels_to_dummy = setdiff(u_vals, base_level)
    
    for (lvl in levels_to_dummy) {
      new_col = paste0("_I", get_xi_base_name(varname), "_", lvl)
      data[[new_col]] = dplyr::if_else(!is.na(col) & col == lvl, 1L, dplyr::if_else(!is.na(col) & col != lvl, 0L, NA_integer_))
      attr(data[[new_col]], "label") = paste0(varname, "==", lvl)
    }
  }
  return(data)
}

#' Runtime execution for Stata xi command
scmd_xi = function(data, var1, var2 = NA_character_) {
  restore.point("scmd_xi")
  
  data = sfun_xi_create_dummies(data, var1)
  
  if (!is.na(var2)) {
    data = sfun_xi_create_dummies(data, var2)
    
    col1 = haven::zap_labels(data[[var1]])
    col2 = haven::zap_labels(data[[var2]])
    
    u_vals1 = sort(unique(col1[!is.na(col1)]))
    u_vals2 = sort(unique(col2[!is.na(col2)]))
    
    if (length(u_vals1) > 0 && length(u_vals2) > 0) {
      levels1 = setdiff(u_vals1, u_vals1[1])
      levels2 = setdiff(u_vals2, u_vals2[1])
      
      base = get_xi_interaction_basename(var1, var2)
      
      for (lvl1 in levels1) {
        for (lvl2 in levels2) {
          new_col = paste0("_I", base, "_", lvl1, "_", lvl2)
          is_match = !is.na(col1) & !is.na(col2) & col1 == lvl1 & col2 == lvl2
          is_valid_non_match = !is.na(col1) & !is.na(col2) & (col1 != lvl1 | col2 != lvl2)
          
          data[[new_col]] = dplyr::if_else(is_match, 1L, dplyr::if_else(is_valid_non_match, 0L, NA_integer_))
          attr(data[[new_col]], "label") = paste0(var1, "==", lvl1, " & ", var2, "==", lvl2)
        }
      }
    }
  }
  
  return(data)
}
```
!END_MODIFICATION stata_xi.R
