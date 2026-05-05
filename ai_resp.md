The diagnosis is correct. The translated condition `dplyr::row_number()==1` is evaluated once on the whole data set, so it keeps only the first row globally. Under Stata’s `by person_id question_nr:`, `_n` and `_N` are evaluated within each by-group, so the R runtime must evaluate the condition group-wise.

Below are focused fixes. I also apply the same logic to `drop if`, because `by group: drop if _n==1` has the same bug pattern.

!MODIFICATION add group-aware condition evaluator in if_in.R
scope = "function"
file = "if_in.R"
insert_after_fun = "s2r_eval_cond"
description = "Add a helper that evaluates translated Stata if conditions separately within by-groups so _n and _N are group-relative."
---------------------------------------------------------------------------------------------------------------------------------------

```r
#' Evaluate a Stata condition string within by-groups
#'
#' This is needed for commands like:
#'   by id: keep if _n == 1
#'   by id: drop if _n == _N
#'
#' The ordinary s2r_eval_cond() evaluates dplyr::row_number() and dplyr::n()
#' against the full data set. Stata evaluates _n and _N within each by-group
#' when a by prefix is active.
s2r_eval_cond_by_group = function(data, r_cond, group_vars = character(0), envir = parent.frame()) {
  restore.point("s2r_eval_cond_by_group")

  if (is.na(r_cond) || r_cond == "") {
    return(rep(TRUE, NROW(data)))
  }

  group_vars = group_vars[!is.na(group_vars) & group_vars != ""]
  group_vars = expand_varlist(paste(group_vars, collapse = " "), names(data))

  if (length(group_vars) == 0) {
    return(s2r_eval_cond(data, r_cond, envir = envir))
  }

  if (NROW(data) == 0) {
    return(logical(0))
  }

  grouped_data = dplyr::group_by(data, !!!dplyr::syms(group_vars), .drop = FALSE)
  group_rows = dplyr::group_rows(grouped_data)

  out = rep(FALSE, NROW(data))

  for (idx in group_rows) {
    idx = as.integer(idx)
    if (length(idx) == 0) next

    sub_data = data[idx, , drop = FALSE]
    out[idx] = s2r_eval_cond(sub_data, r_cond, envir = envir)
  }

  return(fast_coalesce(as.logical(out), FALSE))
}
```

!END_MODIFICATION add group-aware condition evaluator in if_in.R

!MODIFICATION t_keep in t_keep.R
scope = "function"
file = "t_keep.R"
function_name = "t_keep"
description = "Pass by-prefix grouping variables to scmd_keep so _n and _N in keep conditions are evaluated within groups."
---------------------------------------------------------------------------------------------------------------------------

```r
# 2. Code Generation Phase: Translate expressions and emit R code
t_keep = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_keep")

  # Parse the Stata command
  parsed = s2r_p_keep(rest_of_cmd)

  # Translate if expression to R
  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) {
    r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, context)
  }

  # Translate in expression to R numeric range
  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  group_vars_list_bare = character(0)
  if (isTRUE(cmd_obj$is_by_prefix) && !is.na(cmd_obj$by_group_vars) && cmd_obj$by_group_vars != "") {
    group_vars_list = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
    group_vars_list_bare = group_vars_list[!is.na(group_vars_list) & group_vars_list != ""]
  }

  # Build call to runtime execution function `scmd_keep`
  args = c("data = data")
  if (!is.na(parsed$varlist)) {
    args = c(args, paste0("varlist_str = ", quote_for_r_literal(parsed$varlist)))
  }
  if (!is.na(r_if_cond)) {
    args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  }
  if (!is.na(r_in_range)) {
    args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))
  }
  if (length(group_vars_list_bare) > 0) {
    args = c(args, paste0("group_vars = c('", paste(group_vars_list_bare, collapse = "','"), "')"))
  }

  r_code_str = paste0("data = scmd_keep(", paste(args, collapse = ", "), ")")

  # Maintain package internal tracking variables
  if (isTRUE(stata2r_env$has_original_order_idx)) {
    r_code_str = paste0(r_code_str, " %>% \n  dplyr::mutate(stata2r_original_order_idx = dplyr::row_number())")
  }

  return(r_code_str)
}
```

!END_MODIFICATION t_keep in t_keep.R

!MODIFICATION scmd_keep in t_keep.R
scope = "function"
file = "t_keep.R"
function_name = "scmd_keep"
description = "Evaluate keep-if conditions group-wise when a by-prefix supplied grouping variables."
----------------------------------------------------------------------------------------------------

```r
# 3. Runtime Execution Phase: Evaluate against actual data columns and environments
scmd_keep = function(data, varlist_str = NA_character_, r_if_cond = NA_character_, r_in_range = NA_character_, group_vars = character(0)) {
  restore.point("scmd_keep")

  # 1. Row subsetting using if/in.
  # If a by-prefix was active, _n and _N in the condition must be evaluated
  # within each group, not over the whole data set.
  if (!is.na(r_if_cond) && r_if_cond != "") {
    cond_val = s2r_eval_cond_by_group(data, r_if_cond, group_vars = group_vars, envir = parent.frame())
    data = data[cond_val, , drop = FALSE]
  }

  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    data = data[idx, , drop = FALSE]
  }

  # 2. Column subsetting (`varlist`)
  if (!is.na(varlist_str) && varlist_str != "") {
    cols_to_keep = expand_varlist(varlist_str, names(data))
    if (length(cols_to_keep) == 0) {
      stop(paste0("scmd_keep: no variables found matching '", varlist_str, "'"))
    }

    # Always preserve the internal original order index if it exists in the incoming data
    if ("stata2r_original_order_idx" %in% names(data) && !("stata2r_original_order_idx" %in% cols_to_keep)) {
      cols_to_keep = c(cols_to_keep, "stata2r_original_order_idx")
    }

    data = data[, cols_to_keep, drop = FALSE]
  }

  return(data)
}
```

!END_MODIFICATION scmd_keep in t_keep.R

!MODIFICATION t_drop in t_drop.R
scope = "function"
file = "t_drop.R"
function_name = "t_drop"
description = "Pass by-prefix grouping variables to scmd_drop so _n and _N in drop conditions are evaluated within groups."
---------------------------------------------------------------------------------------------------------------------------

```r
# 2. Code Generation Phase: Translate expressions and emit R code
t_drop = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_drop")

  # Parse the Stata command
  parsed = s2r_p_drop(rest_of_cmd)

  # Translate if expression to R
  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) {
    r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, context)
  }

  # Translate in expression to R numeric range
  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  group_vars_list_bare = character(0)
  if (isTRUE(cmd_obj$is_by_prefix) && !is.na(cmd_obj$by_group_vars) && cmd_obj$by_group_vars != "") {
    group_vars_list = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
    group_vars_list_bare = group_vars_list[!is.na(group_vars_list) & group_vars_list != ""]
  }

  # Build call to runtime execution function `scmd_drop`
  args = c("data = data")
  if (!is.na(parsed$varlist)) {
    args = c(args, paste0("varlist_str = ", quote_for_r_literal(parsed$varlist)))
  }
  if (!is.na(r_if_cond)) {
    args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  }
  if (!is.na(r_in_range)) {
    args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))
  }
  if (length(group_vars_list_bare) > 0) {
    args = c(args, paste0("group_vars = c('", paste(group_vars_list_bare, collapse = "','"), "')"))
  }

  r_code_str = paste0("data = scmd_drop(", paste(args, collapse = ", "), ")")

  # Maintain package internal tracking variables
  if (isTRUE(stata2r_env$has_original_order_idx)) {
    r_code_str = paste0(r_code_str, " %>% \n  dplyr::mutate(stata2r_original_order_idx = dplyr::row_number())")
  }

  return(r_code_str)
}
```

!END_MODIFICATION t_drop in t_drop.R

!MODIFICATION scmd_drop in t_drop.R
scope = "function"
file = "t_drop.R"
function_name = "scmd_drop"
description = "Evaluate drop-if conditions group-wise when a by-prefix supplied grouping variables."
----------------------------------------------------------------------------------------------------

```r
# 3. Runtime Execution Phase: Evaluate against actual data columns and environments
scmd_drop = function(data, varlist_str = NA_character_, r_if_cond = NA_character_, r_in_range = NA_character_, group_vars = character(0)) {
  restore.point("scmd_drop")

  # 1. Row dropping (`if` condition).
  # If a by-prefix was active, _n and _N in the condition must be evaluated
  # within each group, not over the whole data set.
  if (!is.na(r_if_cond) && r_if_cond != "") {
    cond_val = s2r_eval_cond_by_group(data, r_if_cond, group_vars = group_vars, envir = parent.frame())
    data = data[!cond_val, , drop = FALSE]
  }

  # 2. Row dropping (`in` range)
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    if (length(idx) > 0) {
      data = data[-idx, , drop = FALSE]
    }
  }

  # 3. Column dropping (`varlist`)
  if (!is.na(varlist_str) && varlist_str != "") {
    cols_to_drop = expand_varlist(varlist_str, names(data))

    # Protect stata2r internal variables from being accidentally dropped by wildcards
    if ("stata2r_original_order_idx" %in% cols_to_drop && !grepl("stata2r_original_order_idx", varlist_str)) {
      cols_to_drop = setdiff(cols_to_drop, "stata2r_original_order_idx")
    }

    if (length(cols_to_drop) > 0) {
      cols_to_keep = setdiff(names(data), cols_to_drop)
      data = data[, cols_to_keep, drop = FALSE]
    }
  }

  return(data)
}
```

!END_MODIFICATION scmd_drop in t_drop.R

With these changes, your problematic line should translate conceptually as:

```r
data = scmd_keep(
  data = data,
  r_if_cond = "dplyr::row_number()==1",
  group_vars = c("person_id", "question_nr")
)
```

The key change is that `s2r_eval_cond_by_group()` applies the existing `s2r_eval_cond()` separately to each group. Since `s2r_eval_cond()` already maps `dplyr::row_number()` to `.stata_row_index` and `dplyr::n()` to `.stata_n_rows`, reusing it on each group makes `_n` and `_N` match Stata’s `by:` semantics.

In your failing example, instead of keeping just the first row of the whole data set, the translated code will now keep the first row for every `person_id question_nr` group, matching:

```stata
by person_id question_nr: keep if _n==1
```
