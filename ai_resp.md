To fix the parsing error for the `at()` option and the deprecation warning for `dplyr::group_indices()`, I will replace the `scmd_egen` function in `t_egen.R`. 

1. **`at()` parsing fix**: The regular expression `\\bat\\s*\\(([^)]+)\\)` incorrectly stopped at the first closing parenthesis `)` when parsing `at(0(10)170)`. I will update this to `\\bat\\s*\\(((?:[^)(]+|\\([^)(]+\\))*)\\)`, which correctly handles one level of nested parentheses (a standard pattern for Stata options like `numlists`).
2. **`dplyr::group_indices()` fix**: Supplying columns via `...` directly to `group_indices(data, !!!...)` is deprecated. Instead, we first apply `dplyr::group_by()` to the dataframe, and then pass the grouped dataframe to `dplyr::group_indices()` without additional arguments.

!MODIFICATION scmd_egen t_egen.R
scope = "function"
file = "R/t_egen.R"
function_name = "scmd_egen"
description = "Fix group_indices() deprecation warning and cut() at() option parsing."
---
```r
scmd_egen = function(data, new_var, func_name, calc_expr, r_if_cond = NA_character_, r_in_range = NA_character_, group_vars = character(0), args_str = NA_character_, needs_temp_sort = FALSE, is_row = FALSE, options_str = NA_character_) {
  restore.point("scmd_egen")

  group_vars_actual = expand_varlist(paste(group_vars, collapse=" "), names(data))

  if (func_name == "group") {
    arg_vars = expand_varlist(args_str, names(data))
    is_missing_opt = fast_coalesce(stringi::stri_detect_regex(options_str, "\\bmissing\\b"), FALSE)

    grouped_data = dplyr::group_by(data, !!!dplyr::syms(arg_vars))
    res = dplyr::group_indices(grouped_data)
    
    if (!is_missing_opt) {
      # Fast NA check across columns to match Stata missing condition
      has_na = Reduce(`|`, lapply(data[arg_vars], is.na))
      res[has_na] = NA_integer_
      res = dplyr::dense_rank(res)
    }

    data$.stata_group_res = res
    calc_expr = "data$.stata_group_res"
    needs_temp_sort = FALSE
    group_vars_actual = character(0) # group evaluates across entire dataset
  } else if (func_name == "cut") {
    arg_vars = expand_varlist(args_str, names(data))
    if (length(arg_vars) != 1) stop("scmd_egen: cut requires exactly one variable")
    var_actual = arg_vars[1]

    at_match = stringi::stri_match_first_regex(options_str, "\\bat\\s*\\(((?:[^)(]+|\\([^)(]+\\))*)\\)")
    group_match = stringi::stri_match_first_regex(options_str, "\\bgroup\\s*\\((\\d+)\\)")
    icodes = fast_coalesce(stringi::stri_detect_regex(options_str, "\\bicodes\\b"), FALSE)

    vec = data[[var_actual]]

    if (!is.na(at_match[1,1])) {
      breaks_str = at_match[1,2]
      breaks_vals = as.numeric(expand_stata_numlist(breaks_str))
      breaks_vals = sort(unique(breaks_vals[!is.na(breaks_vals)]))

      if (length(breaks_vals) < 2) {
         stop(paste0("scmd_egen: cut requires at least two valid breaks, parsed from: ", breaks_str))
      }

      if (icodes) {
        cut_res = as.integer(cut(vec, breaks = breaks_vals, right = FALSE, include.lowest = FALSE, labels = FALSE)) - 1L
      } else {
        lower_bounds = breaks_vals[-length(breaks_vals)]
        cut_res = as.numeric(as.character(cut(vec, breaks = breaks_vals, right = FALSE, include.lowest = FALSE, labels = lower_bounds)))
      }
    } else if (!is.na(group_match[1,1])) {
      num_groups = as.numeric(group_match[1,2])
      probs = seq(0, 1, length.out = num_groups + 1)
      breaks_vals = stats::quantile(vec, probs, na.rm = TRUE)
      breaks_vals[1] = breaks_vals[1] - 1e-6 # ensure lowest is included
      breaks_vals = unique(breaks_vals)

      if (length(breaks_vals) < 2) {
         stop("scmd_egen: cut could not generate sufficient unique quantiles for groups")
      }

      if (icodes) {
        cut_res = as.integer(cut(vec, breaks = breaks_vals, right = TRUE, include.lowest = TRUE, labels = FALSE)) - 1L
      } else {
        lower_bounds = breaks_vals[-length(breaks_vals)]
        cut_res = as.numeric(as.character(cut(vec, breaks = breaks_vals, right = TRUE, include.lowest = TRUE, labels = lower_bounds)))
      }
    } else {
      stop("scmd_egen: cut requires either 'at' or 'group' option")
    }

    data$.stata_cut_res = cut_res
    calc_expr = "data$.stata_cut_res"
  } else if (func_name == "tag") {
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

  # For rank, group, tag, cut where if_cond/in_range usually creates NA for unselected rows
  if (func_name %in% c("rank", "group", "tag", "cut")) {
     data[[new_var]] = dplyr::if_else(data$.stata_temp_mask, data[[new_var]], NA_real_)
  }

  data$.stata_temp_mask = NULL
  data$.stata_group_res = NULL
  data$.stata_cut_res = NULL
  return(data)
}
```
!END_MODIFICATION scmd_egen t_egen.R
