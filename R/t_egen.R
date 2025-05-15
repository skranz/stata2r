# Translate Stata 'egen' command
# Stata: egen [type] newvar = fcn(arguments) [if exp] [in range] [, options]
# Options: by(varlist), etc.

# This is a complex command with many functions.
# We'll implement a few common ones like mean, total, rank.
t_egen = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  # Basic parsing: newvar = function(args) [, by(groupvars)] [if condition]
  # Example: egen mean_i_grp = mean(i), by(group)
  # Example: egen total_i = total(i)
  # Example: bysort group: egen rank_i = rank(i) (Note: bysort handled by cmd_obj$is_by_prefix)

  # Remove type prefix if any
  rest_of_cmd_no_type = stringi::stri_replace_first_regex(rest_of_cmd, "^(?:byte|int|long|float|double)\\s+", "")

  # Regex to get `newvar = func(args)` and optional `if` and `options` (like by())
  # Pattern: newvar = func(var) [if cond] [, options]
  # `^\s*([^=\s]+)\s*=\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*\(([^)]*)\)\s*(?:if\s+(.*?))?\s*(?:,\s*(.*))?$`
  # G1: newvar, G2: func, G3: args_inside_paren, G4: if_cond, G5: options_str

  main_match = stringi::stri_match_first_regex(rest_of_cmd_no_type,
    "^\\s*([^=\\s]+)\\s*=\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*\\(([^)]*)\\)\\s*(?:if\\s+(.*?))?\\s*(?:,(.*))?$")

  if (is.na(main_match[1,1])) {
    return(paste0("# Failed to parse egen command structure: ", rest_of_cmd))
  }

  new_var = stringi::stri_trim_both(main_match[1,2])
  egen_func_name = stringi::stri_trim_both(main_match[1,3])
  egen_args_str = stringi::stri_trim_both(main_match[1,4]) # arguments inside parentheses
  stata_if_cond = stringi::stri_trim_both(main_match[1,5]) # NA if no if
  options_str = stringi::stri_trim_both(main_match[1,6])   # NA if no options like by()

  # Translate expressions/conditions
  r_if_cond = NA_character_
  if (!is.na(stata_if_cond) && stata_if_cond != "") {
    r_if_cond = translate_stata_expression_with_r_values(stata_if_cond, line_num, cmd_df, context = list(is_by_group=FALSE)) # Egen if condition typically outside group
  }

  # Determine by_vars: either from `cmd_obj$by_vars` (if `bysort group: egen...`) or from `options_str` (if `egen ..., by(group)`)
  by_vars_egen = NA_character_
  if (cmd_obj$is_by_prefix && !is.na(cmd_obj$by_vars)) {
    by_vars_egen = cmd_obj$by_vars
  } else if (!is.na(options_str)) {
    by_opt_match = stringi::stri_match_first_regex(options_str, "\\bby\\s*\\(([^)]+)\\)")
    if (!is.na(by_opt_match[1,1])) {
      by_vars_egen = stringi::stri_trim_both(by_opt_match[1,2])
    }
  }

  by_vars_r_list_str = NULL # For dplyr group_by: "var1, var2"
  if (!is.na(by_vars_egen)) {
    by_vars_r_list_str = paste0(stringi::stri_split_regex(by_vars_egen, "\\s+")[[1]], collapse=", ")
  }

  # Translate egen function
  # egen_args_str might be "var1", "var1 var2", or empty for funcs like group()
  # For functions like mean(var1), args_str is "var1". For rowtotal(var1 var2), it's "var1 var2".
  # Translate arguments if they are complex expressions (not typical for egen simple funcs)
  # For now, assume egen_args_str are variable names or simple expressions.
  # `translate_stata_expression_with_r_values` can handle simple variable names too.
  r_egen_args = translate_stata_expression_with_r_values(egen_args_str, line_num, cmd_df, context)


  # Switch for egen functions
  # Resulting expression for mutate: `new_var = R_EQUIVALENT_EXPRESSION`
  mutate_value_expr = ""

  # Common egen functions:
  # mean(var): mean(var, na.rm = TRUE)
  # total(var): sum(var, na.rm = TRUE)
  # count(var): sum(!is.na(var)) or sum(condition)
  # rank(var): min_rank(var)
  # median(var): median(var, na.rm = TRUE)
  # sd(var): sd(var, na.rm = TRUE)
  # group(varlist): group_indices()
  # tag(varlist): !duplicated() after sorting
  # rowtotal(varlist): rowSums for selected vars, NA as 0
  # rowmean(varlist): rowMeans for selected vars, NA ignored

  # For row functions, by_vars_r_list_str should be NULL. They operate row-wise.
  is_row_function = FALSE

  if (egen_func_name == "mean") {
    mutate_value_expr = paste0("mean(", r_egen_args, ", na.rm = TRUE)")
  } else if (egen_func_name == "total" || egen_func_name == "sum") {
    mutate_value_expr = paste0("sum(", r_egen_args, ", na.rm = TRUE)")
  } else if (egen_func_name == "count") {
    # count(exp) counts non-missing results of exp. If exp is varname, sum(!is.na(varname)).
    # If exp is complex, sum(eval(parse(text=r_egen_args)), na.rm=TRUE)
    # For simplicity, assume r_egen_args is a variable name here.
    mutate_value_expr = paste0("sum(!is.na(", r_egen_args, "))") # If egen_args is an expression, this might be sum(eval(parse(text=r_egen_args)) != 0 & !is.na(eval(parse(text=r_egen_args))))
  } else if (egen_func_name == "rank") {
    mutate_value_expr = paste0("dplyr::min_rank(", r_egen_args, ")")
  } else if (egen_func_name == "median" || egen_func_name == "p50") {
    mutate_value_expr = paste0("median(", r_egen_args, ", na.rm = TRUE)")
  } else if (egen_func_name == "sd" || egen_func_name == "std") {
    mutate_value_expr = paste0("sd(", r_egen_args, ", na.rm = TRUE)")
  } else if (egen_func_name == "group") {
    # egen id = group(var1 var2) -> var1 and var2 are in r_egen_args, space separated.
    group_vars_for_func = paste0(stringi::stri_split_regex(r_egen_args, "\\s+")[[1]], collapse=", ")
    mutate_value_expr = paste0("dplyr::group_indices(dplyr::across(dplyr::all_of(c('", paste(stringi::stri_split_regex(r_egen_args, "\\s+")[[1]], collapse="','"), "'))))")
    # For `group()`, by_vars_r_list_str (if any) defines sorting *before* grouping, not grouping itself. This is tricky.
    # Stata: `egen id = group(a b), label`
    # R: `data %>% mutate(id = as.integer(interaction(a,b, drop=TRUE)))` or specific group_indices logic.
    # For now, basic group_indices.
  } else if (egen_func_name == "tag") {
      # egen t = tag(v1 v2) implies sorting by v1 v2 first, then tagging first obs in each group.
      # If `by group: egen t = tag(v1 v2)`, it's within `group`, then by `v1 v2`.
      # This is complex. A simplified version assuming data is already sorted as needed by `by_vars_egen`:
      tag_vars = paste0(stringi::stri_split_regex(r_egen_args, "\\s+")[[1]], collapse=", ")
      mutate_value_expr = paste0("!duplicated(dplyr::select(dplyr::across(dplyr::all_of(c('", paste(stringi::stri_split_regex(r_egen_args, "\\s+")[[1]], collapse="','"), "')))))") # simplified
  } else if (egen_func_name == "rowtotal") {
    vars_for_rowop = paste0(stringi::stri_split_regex(r_egen_args, "\\s+")[[1]], collapse=", ")
    # Stata rowtotal treats NA as 0.
    mutate_value_expr = paste0("rowSums(dplyr::mutate_all(dplyr::select(., ", vars_for_rowop, "), ~tidyr::replace_na(., 0)), na.rm = FALSE)")
    is_row_function = TRUE; by_vars_r_list_str = NULL # Row functions don't use grouping in the same way
  } else if (egen_func_name == "rowmean") {
    vars_for_rowop = paste0(stringi::stri_split_regex(r_egen_args, "\\s+")[[1]], collapse=", ")
    mutate_value_expr = paste0("rowMeans(dplyr::select(., ", vars_for_rowop, "), na.rm = TRUE)")
    is_row_function = TRUE; by_vars_r_list_str = NULL
  } else {
    return(paste0("# Egen function '", egen_func_name, "' not yet implemented."))
  }

  # Combine into a mutate statement
  full_mutate_expr = paste0(new_var, " = ", mutate_value_expr)

  # Apply `if` condition if present (applied *after* group calculation usually)
  # Stata: `egen x = mean(y) if z > 0` calculates mean of y over all obs, then assigns to x if z > 0.
  # Stata: `egen x = mean(y if z > 0)` calculates mean of y where z > 0. (Handled by `r_egen_args` if `y if z>0` is parsed as arg)
  # Here, `if` is on assignment.
  if (!is.na(r_if_cond) && r_if_cond != "") {
    full_mutate_expr = paste0(new_var, " = dplyr::if_else(", r_if_cond, ", ", mutate_value_expr, ", NA)") # Or current value of new_var if exists
  }

  # Build the R command string
  if (!is.null(by_vars_r_list_str) && !is_row_function) {
    r_code_str = paste0("data = data %>%\n  dplyr::group_by(", by_vars_r_list_str, ") %>%\n  dplyr::mutate(", full_mutate_expr, ") %>%\n  dplyr::ungroup()")
  } else {
    r_code_str = paste0("data = dplyr::mutate(data, ", full_mutate_expr, ")")
  }

  return(r_code_str)
}


