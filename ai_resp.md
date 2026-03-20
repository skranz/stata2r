Yes — for this class of cases, I would now treat the row order as inherently non-reproducible unless the Stata code itself makes the sort order unique or explicitly uses `stable`. Stata’s own `sort` documentation says that without `stable`, tied observations are ordered randomly; its merge documentation also warns to use `stable` when within-key order matters. That means your helper column cannot recover “the Stata order” here, because Stata is not preserving prior order in the first place. ([Stata][1])

So I would do two things.

First, revert the `t_sort.R` change that added `stata2r_original_order_idx` as a tie-breaker. That is not Stata semantics, and it can create wrong downstream results for commands that depend on `_n`, lags, or duplicate selection after a nonunique sort. Second, use the comparison-side fix: once a `sort`, `gsort`, or `bysort` can make row order depend on Stata’s tie handling, ignore row order in later dataset comparisons until a command like `use`, `collapse`, or `reshape` establishes a fresh canonical order again. That is the robust fix for your current framework. ([Stata][1])

If you want a later refinement, the next step would be a runtime check that only ignores row order when the actual sort keys are nonunique. But I would first get the robust version working.

!MODIFICATION t_sort.R
scope = "file"
file = "R/t_sort.R"
is_new_file = false
description = "Revert sort behavior to plain sorting without using stata2r_original_order_idx as a tie-breaker, because Stata does not preserve prior order for tied keys by default."
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

```r
# FILE: R/t_sort.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_sort = function(rest_of_cmd) {
  restore.point("s2r_p_sort")
  list(varlist = stringi::stri_trim_both(rest_of_cmd))
}

# 2. Code Generation Phase: Emit R code
t_sort = function(rest_of_cmd, cmd_obj, cmd_df, line_num, type = "sort") {
  restore.point("t_sort")
  parsed = s2r_p_sort(rest_of_cmd)

  if (is.na(parsed$varlist) || parsed$varlist == "") {
    return("# sort/gsort command with no variables specified.")
  }

  args = c("data = data",
           paste0("varlist_str = ", quote_for_r_literal(parsed$varlist)),
           paste0("type = ", quote_for_r_literal(type)))

  r_code_str = paste0("data = scmd_sort(", paste(args, collapse = ", "), ")")

  # Maintain package internal tracking variables (updating after sort)
  if (isTRUE(stata2r_env$has_original_order_idx)) {
    r_code_str = paste0(r_code_str, " %>% \n  dplyr::mutate(stata2r_original_order_idx = dplyr::row_number())")
  }

  return(r_code_str)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_sort = function(data, varlist_str, type = "sort") {
  restore.point("scmd_sort")

  tokens = stringi::stri_split_regex(trimws(varlist_str), "\\s+")[[1]]
  tokens = tokens[tokens != ""]

  if (length(tokens) == 0) return(data)

  sort_exprs = character(0)

  # Resolve +/- and wildcards correctly
  for (tok in tokens) {
    desc = FALSE
    if (type == "gsort") {
      if (startsWith(tok, "-")) {
        desc = TRUE
        tok = substring(tok, 2)
      } else if (startsWith(tok, "+")) {
        tok = substring(tok, 2)
      }
    }

    expanded = expand_varlist(tok, names(data))
    for (v in expanded) {
      if (desc) {
        sort_exprs = c(sort_exprs, paste0("dplyr::desc(`", v, "`)"))
      } else {
        sort_exprs = c(sort_exprs, paste0("`", v, "`"))
      }
    }
  }

  if (length(sort_exprs) > 0) {
    cmd = paste0("dplyr::arrange(data, ", paste(sort_exprs, collapse = ", "), ")")
    data = eval(parse(text = cmd), envir = list(data = data), enclos = parent.frame())
  }

  return(data)
}
```

!END_MODIFICATION t_sort.R

!MODIFICATION mark_data_manip_cmd.R
scope = "file"
file = "R/mark_data_manip_cmd.R"
is_new_file = false
description = "Propagate ignore_row_order_for_comparison after sort-like commands, including bysort, until commands such as use, collapse, or reshape reset the dataset order."
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

```r
s2r_cmd_resets_row_order_compare = function(cmd_obj) {
  stata_cmd = cmd_obj$stata_cmd[1]
  if (is.na(stata_cmd) || stata_cmd == "") return(FALSE)

  stata_cmd %in% c("use", "collapse", "reshape")
}

s2r_cmd_can_change_row_order = function(cmd_obj) {
  stata_cmd = cmd_obj$stata_cmd[1]
  if (is.na(stata_cmd) || stata_cmd == "") return(FALSE)

  if (stata_cmd %in% c("sort", "gsort")) {
    return(TRUE)
  }

  isTRUE(cmd_obj$is_bysort_prefix[1])
}

mark_data_manip_cmd = function(cmd_df) {
  restore.point("mark_data_manip_cmd")

  if (NROW(cmd_df) == 0) {
    cmd_df = s2r_check_mod_df(cmd_df)
    if (!("will_have_original_order_idx" %in% names(cmd_df))) cmd_df$will_have_original_order_idx = logical(0)
    if (!("will_ignore_row_order_for_comparison" %in% names(cmd_df))) cmd_df$will_ignore_row_order_for_comparison = logical(0)
    return(cmd_df)
  }

  cmd_df = s2r_check_mod_df(cmd_df)

  if (!("will_have_original_order_idx" %in% names(cmd_df))) {
    cmd_df$will_have_original_order_idx = rep(FALSE, NROW(cmd_df))
  }
  cmd_df$will_ignore_row_order_for_comparison = rep(FALSE, NROW(cmd_df))

  current_has_order_idx_at_translation_time = FALSE
  row_order_may_differ_from_stata = FALSE

  for (i in seq_len(NROW(cmd_df))) {
    cmd_obj = cmd_df[i, , drop = FALSE]

    if (s2r_cmd_resets_row_order_compare(cmd_obj)) {
      row_order_may_differ_from_stata = FALSE
    }

    if (cmd_df$do_translate[i]) {
      if (cmd_df$stata_cmd[i] %in% c("use", "collapse", "reshape")) {
        current_has_order_idx_at_translation_time = TRUE
      } else if (cmd_df$stata_cmd[i] %in% c("drop", "keep", "expand", "merge", "append", "order")) {
        current_has_order_idx_at_translation_time = current_has_order_idx_at_translation_time
      } else if (cmd_df$stata_cmd[i] %in% c("preserve")) {
        current_has_order_idx_at_translation_time = current_has_order_idx_at_translation_time
      } else if (cmd_df$stata_cmd[i] %in% c("restore")) {
        current_has_order_idx_at_translation_time = current_has_order_idx_at_translation_time
      }
    }
    cmd_df$will_have_original_order_idx[i] = current_has_order_idx_at_translation_time

    if (s2r_cmd_can_change_row_order(cmd_obj)) {
      row_order_may_differ_from_stata = TRUE
    }

    cmd_df$will_ignore_row_order_for_comparison[i] = row_order_may_differ_from_stata
  }

  return(cmd_df)
}
```

!END_MODIFICATION mark_data_manip_cmd.R

So my recommendation is: revert the sort tie-break change, apply the row-order-ignore propagation, and only come back to a more selective runtime-based rule later.

[1]: https://www.stata.com/manuals/dsort.pdf?utm_source=chatgpt.com "Sort data"
