# Translate Stata 'drop' command
# Can be `drop varlist` or `drop if condition` or `drop in range`

t_drop = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_drop") # Added restore.point
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  is_if_drop = stringi::stri_startswith_fixed(rest_of_cmd_trimmed, "if ")
  is_in_drop = stringi::stri_startswith_fixed(rest_of_cmd_trimmed, "in ")

  if (is_if_drop) {
    # drop if condition
    stata_if_cond = stringi::stri_sub(rest_of_cmd_trimmed, from = 4)
    r_if_cond = translate_stata_expression_with_r_values(stata_if_cond, line_num, cmd_df, context)
    # Using dplyr::filter, treating NA in condition as FALSE (Stata behavior)
    r_code_str = paste0("data = dplyr::filter(data, !(dplyr::coalesce(as.numeric(", r_if_cond, "), 0) != 0))")
  } else if (is_in_drop) {
    # drop in range
    range_str = stringi::stri_sub(rest_of_cmd_trimmed, from = 4)
    range_match = stringi::stri_match_first_regex(range_str, "^(\\d+)(?:/(\\d+))?$")
    if (!is.na(range_match[1,1])) {
      start_row = as.integer(range_match[1,2])
      end_row = range_match[1,3]
      if (is.na(end_row)) {
        slice_expr = paste0("-",start_row) # Drop single row
      } else {
        slice_expr = paste0("-(", start_row, ":", as.integer(end_row), ")") # Drop range
      }
      # Using dplyr::slice
      r_code_str = paste0("data = dplyr::slice(data, ", slice_expr, ")")
    } else {
      r_code_str = paste0("# drop in range '", range_str, "' not fully translated (f/l specifiers).")
    }
  } else {
    # drop varlist
    vars_to_drop_raw = stringi::stri_split_regex(rest_of_cmd_trimmed, "\\s+")[[1]]
    vars_to_drop_raw = vars_to_drop_raw[vars_to_drop_raw != ""]

    if (length(vars_to_drop_raw) == 0) {
      return("# drop command with no variables specified.")
    }

    # Check for wildcards in any of the variable names
    has_wildcard = any(stringi::stri_detect_fixed(vars_to_drop_raw, "*") | stringi::stri_detect_fixed(vars_to_drop_raw, "?"))

    if (has_wildcard) {
        # Construct expressions for dplyr::select using matches() and all_of()
        select_exprs = character(0)
        for (var_pattern in vars_to_drop_raw) {
            if (stringi::stri_detect_fixed(var_pattern, "*") || stringi::stri_detect_fixed(var_pattern, "?")) {
                # Convert Stata wildcards to regex
                regex_pattern = stringi::stri_replace_all_fixed(var_pattern, "*", ".*")
                regex_pattern = stringi::stri_replace_all_fixed(regex_pattern, "?", ".")
                # Ensure it matches whole variable names by adding anchors
                select_exprs = c(select_exprs, paste0("dplyr::matches('^", regex_pattern, "$')"))
            } else {
                select_exprs = c(select_exprs, paste0("dplyr::all_of('", var_pattern, "')"))
            }
        }
        r_code_str = paste0("data = dplyr::select(data, -c(", paste(select_exprs, collapse=", "), "))")
    } else {
        # No wildcards, use all_of directly for efficiency and clarity
        r_code_str = paste0("data = dplyr::select(data, -dplyr::all_of(c('", paste(vars_to_drop_raw, collapse="','"), "')))")
    }
  }

  return(r_code_str)
}

