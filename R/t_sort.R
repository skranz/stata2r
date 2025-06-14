# Translate Stata 'sort' and 'gsort' commands
# Stata: sort varlist
# Stata: gsort [+|-]varname [[+|-]varname ...]
t_sort = function(rest_of_cmd, cmd_obj, cmd_df, line_num, type = "sort") {
  restore.point("t_sort") # Added restore.point
  if (is.na(rest_of_cmd) || rest_of_cmd == "") {
    return("# sort/gsort command with no variables specified.")
  }

  varlist = stringi::stri_trim_both(rest_of_cmd)
  vars = stringi::stri_split_regex(varlist, "\\s+")[[1]]
  vars = vars[vars != ""] # Filter out empty strings from splitting

  if (length(vars) == 0) {
    return("# sort/gsort command with no effectively parsed variables.")
  }

  # Determine if stata2r_original_order_idx should be used as a tie-breaker
  # This decision is made at translation time based on `will_have_original_order_idx` flag.
  # For direct 'sort' and 'gsort' commands, Stata does NOT guarantee stable sort for ties.
  # Thus, we should NOT add stata2r_original_order_idx as a tie-breaker here.
  # The comparison function will handle row order differences for these commands.
  # If a specific `stable` option were supported in Stata, this logic might change.
  # For now, `use_original_order_idx_for_this_sort` will be FALSE for `t_sort`.
  # The `t_egen` function will still use the index for internal sorting if needed.
  use_original_order_idx_for_this_sort = FALSE # Override: Stata's sort is not stable by default.

  if (type == "sort") {
    sort_vars = vars
    if (use_original_order_idx_for_this_sort) { # This will now be FALSE
      sort_vars = c(sort_vars, "stata2r_original_order_idx")
    }
    # Using dplyr::arrange with !!!dplyr::syms for consistency and robustness
    sort_vars_r = paste0('!!!dplyr::syms(c("', paste(sort_vars, collapse='", "'), '"))')
    r_code_str = paste0("data = dplyr::arrange(data, ", sort_vars_r, ")")

  } else if (type == "gsort") {
    # gsort allows specifying ascending (+) or descending (-) for each variable
    # +var (ascending, default if no sign)
    # -var (descending)
    # dplyr: arrange(var1, desc(var2), ...)
    arrange_expressions = character(length(vars))
    for (i in seq_along(vars)) {
      var_spec = vars[i]
      if (stringi::stri_startswith_fixed(var_spec, "-")) {
        var_name = stringi::stri_sub(var_spec, 2)
        arrange_expressions[i] = paste0("dplyr::desc(!!!dplyr::syms(\"", var_name, "\"))")
      } else if (stringi::stri_startswith_fixed(var_spec, "+")) {
        var_name = stringi::stri_sub(var_spec, 2)
        arrange_expressions[i] = paste0("!!!dplyr::syms(\"", var_name, "\")")
      } else {
        arrange_expressions[i] = paste0("!!!dplyr::syms(\"", var_spec, "\")")
      }
    }
    # Add stata2r_original_order_idx as the final tie-breaker to ensure stable sort for ties
    if (use_original_order_idx_for_this_sort) { # This will now be FALSE
      arrange_expressions = c(arrange_expressions, '!!!dplyr::syms("stata2r_original_order_idx")')
    }
    r_code_str = paste0("data = dplyr::arrange(data, ", paste(arrange_expressions, collapse = ", "), ")")
  } else {
    r_code_str = paste0("# Unknown sort type: ", type)
  }

  # Update stata2r_original_order_idx to reflect the new row order/count
  # This check still uses the runtime flag, which is correct for updating the column itself.
  if (isTRUE(stata2r_env$has_original_order_idx)) {
    r_code_str = paste0(r_code_str, " %>% \n  dplyr::mutate(stata2r_original_order_idx = dplyr::row_number())")
  }

  return(r_code_str)
}


