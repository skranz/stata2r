


### Analysis of the Problem

1.  **The Error:**
    The traceback in the `test_report.Rmd` file points to an error during `drf_get_data` executing the translated R code: `object 'cutoff' not found` while evaluating `sfun_stata_cond(x1 > cutoff, cutoff, x1)` inside a `dplyr::mutate` operation. 

2.  **The Cause:**
    *   The package was missing the implementation for translating Stata's `scalar` command (`t_scalar.R`). As a result, the R translation engine encountered an error parsing the `scalar cutoff = 10.5` command and emitted a commented-out fallback line. Consequently, the variable `cutoff` was never instantiated in the R execution environment (`exec_env`). 
    *   When the subsequent `gen x1_capped = cond(x1 > cutoff, cutoff, x1)` command was executed, `dplyr::mutate` correctly looked up `cutoff` within its dataset and then enclosing environments (`exec_env`). Finding nothing, it crashed.
    *   A secondary, hidden issue relates to condition filtering using scalars (e.g. `reg y x1 x2 if year > min_year`). `scmd_keep` and other data modification commands use `s2r_eval_cond` to evaluate the translated string condition `r_if_cond`. Currently, `s2r_eval_cond` hardcodes `enclos = parent.frame()` which refers to its own local execution context inside the `stata2r` package, effectively isolating it from user-defined variables residing in `exec_env`.

### Solutions

1.  **Add `t_scalar.R`**: This module translates `scalar` definitions into standard R assignments executed natively in `exec_env`. Because `dplyr::mutate` searches enclosing environments, variables generated in `exec_env` will map exactly to Stata's fallback resolution rules.
2.  **Propagate the execution environment (`envir`)**: Expand `s2r_eval_cond` to optionally accept an evaluation environment (defaulting to `parent.frame()`). Then pass `parent.frame()` recursively down from the entrypoint `scmd_*` functions (like `scmd_keep`, `scmd_drop`, etc.) so that string-evaluated filter expressions correctly locate execution environment scalars.

### Suggested Improvements for Test Reporting

-   **Log Translated R Code:** In `drf_run_r.R` during test execution, wrap `eval(rcode_call, envir = exec_env)` in a `tryCatch()`. If it fails, attach the generated `rcode` payload to the error string. Currently, debugging requires guessing what `rcode_call` contained.
-   **Show Execution Context Variables:** Dumping `ls(exec_env)` to the test report alongside the traceback when a pipeline translation collapses could save enormous amounts of debugging time by showing missing (or accidentally overwritten) variables.

---

### Modifications

!MODIFICATION t_scalar.R
scope = "file"
file = "/home/rstudio/aicoder/stata2r/R/t_scalar.R"
description = "Create t_scalar.R to translate Stata scalar commands."
---
```r
# FILE: /home/rstudio/aicoder/stata2r/R/t_scalar.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_scalar = function(rest_of_cmd) {
  restore.point("s2r_p_scalar")
  rest = stringi::stri_trim_both(stringi::stri_replace_first_regex(rest_of_cmd, "^define\\s+", ""))
  parts = stringi::stri_split_fixed(rest, "=", n = 2)[[1]]
  if (length(parts) == 2) {
    return(list(
      name = stringi::stri_trim_both(parts[1]),
      expr = stringi::stri_trim_both(parts[2])
    ))
  }
  return(list(name = NA_character_, expr = NA_character_))
}

# 2. Code Generation Phase: Emit R code
t_scalar = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_scalar")
  parsed = s2r_p_scalar(rest_of_cmd)
  if (is.na(parsed$name)) return(paste0("# Failed to parse scalar: ", rest_of_cmd))
  
  r_expr = translate_stata_expression_with_r_values(parsed$expr, line_num, cmd_df, list(is_by_group = FALSE))
  
  args = c("data = data", paste0("r_expr_str = ", quote_for_r_literal(r_expr)))
  
  return(paste0("`", parsed$name, "` = scmd_scalar(", paste(args, collapse = ", "), ")"))
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_scalar = function(data, r_expr_str) {
  restore.point("scmd_scalar")
  eval_list = s2r_setup_eval_list(data)
  res = base::eval(base::parse(text = r_expr_str), envir = eval_list, enclos = parent.frame())
  if (length(res) > 0) res[1] else NA_real_
}
```
!END_MODIFICATION t_scalar.R


!MODIFICATION s2r_eval_cond in /home/rstudio/aicoder/stata2r/R/if_in.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/if_in.R"
function_name = "s2r_eval_cond"
description = "Update s2r_eval_cond to accept envir parameter to resolve scalars from enclosing environment"
---
```r
#' Evaluate a Stata condition string at runtime, returning a boolean vector
s2r_eval_cond = function(data, r_cond, envir = parent.frame()) {
  restore.point("s2r_eval_cond")
  if (is.na(r_cond) || r_cond == "") return(rep(TRUE, NROW(data)))

  r_cond = s2r_prepare_runtime_cond(r_cond)

  expr = base::parse(text = r_cond)
  used_vars = all.vars(expr)
  data_cols = names(data)

  eval_list = s2r_setup_eval_list(data)
  eval_list[[".stata_row_index"]] = seq_len(NROW(data))
  eval_list[[".stata_n_rows"]] = NROW(data)

  reserved = c(s2r_eval_reserved_words(), ".stata_row_index", ".stata_n_rows")

  for (v in used_vars) {
    if (!(v %in% c(data_cols, reserved))) {
      mcols = data_cols[startsWith(data_cols, v)]
      if (length(mcols) == 1) {
        eval_list[[v]] = to_stata_na(data[[mcols[1]]])
      } else if (length(mcols) > 1) {
        stop(paste("Ambiguous abbreviation in condition:", v))
      }
    }
  }

  cond_val = base::eval(expr, envir = eval_list, enclos = envir)
  return(dplyr::coalesce(as.logical(cond_val), FALSE))
}
```
!END_MODIFICATION s2r_eval_cond in /home/rstudio/aicoder/stata2r/R/if_in.R


!MODIFICATION s2r_eval_if_in in /home/rstudio/aicoder/stata2r/R/if_in.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/if_in.R"
function_name = "s2r_eval_if_in"
description = "Update s2r_eval_if_in to accept envir parameter and pass it to s2r_eval_cond"
---
```r
#' Safely evaluate if/in conditions at runtime for row keeping
#' @param data The dataframe
#' @param r_if_cond The translated R condition string
#' @param r_in_range The translated R range string
#' @param envir The enclosing environment for evaluation
#' @return The subsetted dataframe
s2r_eval_if_in = function(data, r_if_cond = NA_character_, r_in_range = NA_character_, envir = parent.frame()) {
  restore.point("seval_if_in")

  if (!is.na(r_if_cond) && r_if_cond != "") {
    cond_val = s2r_eval_cond(data, r_if_cond, envir = envir)
    data = data[cond_val, , drop = FALSE]
  }

  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    data = data[idx, , drop = FALSE]
  }

  return(data)
}
```
!END_MODIFICATION s2r_eval_if_in in /home/rstudio/aicoder/stata2r/R/if_in.R


!MODIFICATION scmd_drop in /home/rstudio/aicoder/stata2r/R/t_drop.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/t_drop.R"
function_name = "scmd_drop"
description = "Pass parent.frame() to s2r_eval_cond in scmd_drop"
---
```r
# 3. Runtime Execution Phase: Evaluate against actual data columns and environments
scmd_drop = function(data, varlist_str = NA_character_, r_if_cond = NA_character_, r_in_range = NA_character_) {
  restore.point("scmd_drop")

  # 1. Row dropping (`if` condition)
  if (!is.na(r_if_cond) && r_if_cond != "") {
    cond_val = s2r_eval_cond(data, r_if_cond, envir = parent.frame())
    # Inverse of keep: we retain FALSE / NAs
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
!END_MODIFICATION scmd_drop in /home/rstudio/aicoder/stata2r/R/t_drop.R


!MODIFICATION scmd_keep in /home/rstudio/aicoder/stata2r/R/t_keep.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/t_keep.R"
function_name = "scmd_keep"
description = "Pass parent.frame() to s2r_eval_if_in in scmd_keep"
---
```r
# 3. Runtime Execution Phase: Evaluate against actual data columns and environments
scmd_keep = function(data, varlist_str = NA_character_, r_if_cond = NA_character_, r_in_range = NA_character_) {
  restore.point("scmd_keep")

  # 1. Row subsetting using generalized if/in evaluator
  data = s2r_eval_if_in(data, r_if_cond, r_in_range, envir = parent.frame())

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
!END_MODIFICATION scmd_keep in /home/rstudio/aicoder/stata2r/R/t_keep.R


!MODIFICATION scmd_collapse in /home/rstudio/aicoder/stata2r/R/t_collapse.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/t_collapse.R"
function_name = "scmd_collapse"
description = "Pass parent.frame() to s2r_eval_cond in scmd_collapse"
---
```r
# 3. Runtime Execution Phase
scmd_collapse = function(data, agg_exprs_list, group_vars = character(0), r_if_cond = NA_character_) {
  restore.point("scmd_collapse")
  if (!is.na(r_if_cond) && r_if_cond != "") data = data[s2r_eval_cond(data, r_if_cond, envir = parent.frame()), , drop = FALSE]

  pipe_el = c("data")
  group_vars_actual = expand_varlist(paste(group_vars, collapse=" "), names(data))
  if (length(group_vars_actual) > 0) pipe_el = c(pipe_el, paste0("collapse::fgroup_by(", paste(group_vars_actual, collapse=", "), ")"))

  agg_str = paste(sprintf("`%s` = %s", names(agg_exprs_list), unlist(agg_exprs_list)), collapse = ", ")
  pipe_el = c(pipe_el, paste0("collapse::fsummarise(", agg_str, ")"))
  if (length(group_vars_actual) > 0) pipe_el = c(pipe_el, "collapse::fungroup()")

  data = eval(parse(text = paste(pipe_el, collapse = " %>% ")), envir = list(data = data), enclos = parent.frame())
  data$stata2r_original_order_idx = seq_len(nrow(data))
  return(data)
}
```
!END_MODIFICATION scmd_collapse in /home/rstudio/aicoder/stata2r/R/t_collapse.R


!MODIFICATION scmd_decode in /home/rstudio/aicoder/stata2r/R/t_decode.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/t_decode.R"
function_name = "scmd_decode"
description = "Pass parent.frame() to s2r_eval_cond in scmd_decode"
---
```r
# 3. Runtime Execution Phase: Evaluate against actual data
scmd_decode = function(data, varname, gen_var, r_if_cond = NA_character_, r_in_range = NA_character_) {
  restore.point("scmd_decode")
  var_actual = expand_varlist(varname, names(data))[1]

  mask = rep(TRUE, nrow(data))
  if (!is.na(r_if_cond) && r_if_cond != "") mask = mask & s2r_eval_cond(data, r_if_cond, envir = parent.frame())
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask = rep(FALSE, nrow(data))
    in_mask[idx] = TRUE
    mask = mask & in_mask
  }

  src = data[[var_actual]]
  decoded = as.character(haven::as_factor(src))
  decoded = dplyr::if_else(is.na(decoded), dplyr::if_else(is.na(src), "", as.character(src)), decoded)

  if (!(gen_var %in% names(data))) data[[gen_var]] = NA_character_
  data[[gen_var]] = dplyr::if_else(mask, decoded, data[[gen_var]])

  return(data)
}
```
!END_MODIFICATION scmd_decode in /home/rstudio/aicoder/stata2r/R/t_decode.R


!MODIFICATION scmd_destring in /home/rstudio/aicoder/stata2r/R/t_destring.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/t_destring.R"
function_name = "scmd_destring"
description = "Pass parent.frame() to s2r_eval_cond in scmd_destring"
---
```r
# 3. Runtime Execution Phase: Evaluate against actual data
scmd_destring = function(data, varlist_str, is_replace, gen_vars_str = NA_character_, r_if_cond = NA_character_, r_in_range = NA_character_) {
  restore.point("scmd_destring")
  vars_actual = expand_varlist(varlist_str, names(data))

  if (is_replace) {
    new_vars = vars_actual
  } else {
    new_vars = stringi::stri_split_regex(gen_vars_str, "\\s+")[[1]]
    new_vars = new_vars[new_vars != ""]
    if (length(new_vars) != length(vars_actual)) {
      stop("scmd_destring: generate() requires same number of new variables as old variables.")
    }
  }

  mask = rep(TRUE, nrow(data))
  if (!is.na(r_if_cond) && r_if_cond != "") mask = mask & s2r_eval_cond(data, r_if_cond, envir = parent.frame())
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask = rep(FALSE, nrow(data))
    in_mask[idx] = TRUE
    mask = mask & in_mask
  }

  for (i in seq_along(vars_actual)) {
    old_v = vars_actual[i]
    new_v = new_vars[i]
    destrung = suppressWarnings(readr::parse_number(as.character(data[[old_v]])))

    if (is_replace) {
      data[[new_v]] = dplyr::if_else(mask, destrung, data[[old_v]])
    } else {
      res = rep(NA_real_, nrow(data))
      res[mask] = destrung[mask]
      data[[new_v]] = res
    }
  }

  return(data)
}
```
!END_MODIFICATION scmd_destring in /home/rstudio/aicoder/stata2r/R/t_destring.R


!MODIFICATION scmd_duplicates in /home/rstudio/aicoder/stata2r/R/t_duplicates.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/t_duplicates.R"
function_name = "scmd_duplicates"
description = "Pass parent.frame() to s2r_eval_cond in scmd_duplicates"
---
```r
# 3. Runtime Execution Phase: Evaluate against actual data
scmd_duplicates = function(data, subcommand, varlist_str = NA_character_, gen_var = NA_character_, r_if_cond = NA_character_, r_in_range = NA_character_) {
  restore.point("scmd_duplicates")
  mask = rep(TRUE, nrow(data))
  if (!is.na(r_if_cond) && r_if_cond != "") mask = mask & s2r_eval_cond(data, r_if_cond, envir = parent.frame())
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask = rep(FALSE, nrow(data))
    in_mask[idx] = TRUE
    mask = mask & in_mask
  }

  cols = names(data)
  if (!is.na(varlist_str) && varlist_str != "") {
    cols = expand_varlist(varlist_str, names(data))
  }

  is_dup = base::duplicated(data[, cols, drop = FALSE], fromLast = FALSE)

  if (subcommand == "drop") {
    data = data[!(is_dup & mask), , drop = FALSE]
    rownames(data) = NULL
  } else if (subcommand == "tag") {
    if (is.na(gen_var)) stop("duplicates tag requires gen_var")
    is_first = !is_dup
    data[[gen_var]] = dplyr::if_else(is_first & mask, 1, 0)
  } else if (subcommand == "list") {
    print(data[is_dup & mask, , drop = FALSE])
  }

  return(data)
}
```
!END_MODIFICATION scmd_duplicates in /home/rstudio/aicoder/stata2r/R/t_duplicates.R


!MODIFICATION scmd_expand in /home/rstudio/aicoder/stata2r/R/t_expand.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/t_expand.R"
function_name = "scmd_expand"
description = "Pass parent.frame() to s2r_eval_cond in scmd_expand"
---
```r
# 3. Runtime Execution Phase: Evaluate against actual data
scmd_expand = function(data, r_n_expr_str, r_if_cond = NA_character_, r_in_range = NA_character_) {
  restore.point("scmd_expand")
  mask = rep(TRUE, nrow(data))
  if (!is.na(r_if_cond) && r_if_cond != "") mask = mask & s2r_eval_cond(data, r_if_cond, envir = parent.frame())
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask = rep(FALSE, nrow(data))
    in_mask[idx] = TRUE
    mask = mask & in_mask
  }

  # Evaluate N expression inside data context properly
  eval_list = s2r_setup_eval_list(data)
  n_vals = base::eval(base::parse(text = r_n_expr_str), envir = eval_list, enclos = parent.frame())

  final_times = ifelse(mask, ifelse(is.na(n_vals), 1, pmax(0, as.integer(n_vals))), 1)

  data = data[rep(1:nrow(data), times = final_times), ]
  rownames(data) = NULL

  return(dplyr::as_tibble(data))
}
```
!END_MODIFICATION scmd_expand in /home/rstudio/aicoder/stata2r/R/t_expand.R


!MODIFICATION scmd_summarize in /home/rstudio/aicoder/stata2r/R/t_summarize.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/t_summarize.R"
function_name = "scmd_summarize"
description = "Pass parent.frame() to s2r_eval_cond in scmd_summarize"
---
```r
# 3. Runtime Execution Phase
scmd_summarize = function(data, needed_r, var_for_r = NA_character_, r_if_cond = NA_character_) {
  restore.point("scmd_summarize")
  if (!is.na(r_if_cond) && r_if_cond != "") {
    data = data[s2r_eval_cond(data, r_if_cond, envir = parent.frame()), , drop = FALSE]
  }

  res = list()
  if (is.na(var_for_r)) {
    if ("r(N)" %in% needed_r) {
      res$r_N = nrow(data)
    }
  } else {
    v = expand_varlist(var_for_r, names(data))[1]
    col = data[[v]]
    if ("r(N)" %in% needed_r) {
      res$r_N = sum(!is.na(col))
    }
    if ("r(mean)" %in% needed_r) {
      res$r_mean = mean(col, na.rm = TRUE)
    }
    if ("r(sd)" %in% needed_r) {
      res$r_sd = sd(col, na.rm = TRUE)
    }
    if ("r(min)" %in% needed_r) {
      res$r_min = min(col, na.rm = TRUE)
    }
    if ("r(max)" %in% needed_r) {
      res$r_max = max(col, na.rm = TRUE)
    }
    if ("r(sum)" %in% needed_r) {
      res$r_sum = sum(col, na.rm = TRUE)
    }
    if ("r(p50)" %in% needed_r) {
      res$r_p50 = median(col, na.rm = TRUE)
    }
  }
  return(res)
}
```
!END_MODIFICATION scmd_summarize in /home/rstudio/aicoder/stata2r/R/t_summarize.R


!MODIFICATION scmd_encode in /home/rstudio/aicoder/stata2r/R/t_encode.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/t_encode.R"
function_name = "scmd_encode"
description = "Pass parent.frame() to s2r_eval_cond in scmd_encode"
---
```r
# 3. Runtime Execution Phase: Evaluate against actual data
scmd_encode = function(data, varname, gen_var, r_if_cond = NA_character_, r_in_range = NA_character_) {
  restore.point("scmd_encode")
  var_actual = expand_varlist(varname, names(data))[1]

  mask = rep(TRUE, nrow(data))
  if (!is.na(r_if_cond) && r_if_cond != "") mask = mask & s2r_eval_cond(data, r_if_cond, envir = parent.frame())
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask = rep(FALSE, nrow(data))
    in_mask[idx] = TRUE
    mask = mask & in_mask
  }

  src = data[[var_actual]]
  uq_vals = sort(unique(src[!is.na(src)]))
  num_vals = match(src, uq_vals)
  labels_map = stats::setNames(as.numeric(1:length(uq_vals)), uq_vals)

  encoded = haven::labelled(as.integer(num_vals), labels = labels_map)

  if (!(gen_var %in% names(data))) data[[gen_var]] = NA_integer_
  data[[gen_var]] = dplyr::if_else(mask, encoded, data[[gen_var]])

  return(data)
}
```
!END_MODIFICATION scmd_encode in /home/rstudio/aicoder/stata2r/R/t_encode.R


!MODIFICATION scmd_estimation_effects in /home/rstudio/aicoder/stata2r/R/t_estimation_cmd.R
scope = "function"
file = "/home/rstudio/aicoder/stata2r/R/t_estimation_cmd.R"
function_name = "scmd_estimation_effects"
description = "Pass parent.frame() to s2r_eval_cond in scmd_estimation_effects"
---
```r
scmd_estimation_effects = function(data, dep_var, model_vars, needed_e, r_if_cond = NA_character_, estimator = "regress", formula_terms = character(0)) {
  restore.point("scmd_estimation_effects")

  mask = rep(TRUE, nrow(data))
  if (!is.na(r_if_cond) && r_if_cond != "") {
    mask = mask & s2r_eval_cond(data, r_if_cond, envir = parent.frame())
  }

  dep_actual = expand_varlist(dep_var, names(data))
  if (length(dep_actual) > 0) dep_actual = dep_actual[1] else dep_actual = character(0)

  model_vars_actual = character(0)
  if (length(model_vars) > 0) {
    model_vars_actual = unlist(lapply(model_vars, function(v) {
      expanded = expand_varlist(v, names(data))
      if (length(expanded) > 0) expanded else character(0)
    }))
  }
  model_vars_actual = unique(model_vars_actual)

  all_vars = unique(c(dep_actual, model_vars_actual))
  if (length(all_vars) > 0) {
    cc_mask = stats::complete.cases(data[, all_vars, drop = FALSE])
  } else {
    cc_mask = rep(TRUE, nrow(data))
  }

  e_sample = as.integer(mask & cc_mask)
  res = list(e_sample = e_sample)

  if ("e(N)" %in% needed_e) {
    res$e_N = sum(e_sample)
  }

  other_e = setdiff(needed_e, c("e(sample)", "e(N)"))
  if (length(other_e) > 0) {
    can_fit_lm = estimator %in% c("regress", "areg", "xtreg", "reghdfe", "ivregress") &&
      length(formula_terms) > 0 &&
      all(stringi::stri_detect_regex(formula_terms, "^[A-Za-z_][A-Za-z0-9_]*$"))

    if (can_fit_lm && sum(e_sample) > 0) {
      # Use only the first matched column for the dependent variable in formula
      formula_str = paste0("`", dep_actual, "` ~ ", paste(paste0("`", formula_terms, "`"), collapse = " + "))
      mod = stats::lm(stats::as.formula(formula_str), data = data[e_sample == 1, , drop = FALSE])
      sm = summary(mod)

      if ("e(r2)" %in% needed_e) res$e_r2 = sm$r.squared
      if ("e(df_r)" %in% needed_e) res$e_df_r = mod$df.residual
      if ("e(rmse)" %in% needed_e) res$e_rmse = sm$sigma
    } else {
      if ("e(r2)" %in% needed_e) res$e_r2 = NA_real_
      if ("e(df_r)" %in% needed_e) res$e_df_r = NA_real_
      if ("e(rmse)" %in% needed_e) res$e_rmse = NA_real_
    }
  }

  return(res)
}
```
!END_MODIFICATION scmd_estimation_effects in /home/rstudio/aicoder/stata2r/R/t_estimation_cmd.R
