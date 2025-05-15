# Translate Stata 'keep' command
# Can be `keep varlist` or `keep if condition` or `keep in range`

t_keep = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {

  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # Try to determine type of keep:
  is_if_keep = stringi::stri_startswith_fixed(rest_of_cmd_trimmed, "if ")
  is_in_keep = stringi::stri_startswith_fixed(rest_of_cmd_trimmed, "in ")

  if (is_if_keep) {
    # keep if condition
    stata_if_cond = stringi::stri_sub(rest_of_cmd_trimmed, from = 4) # Length of "if " is 3, +1 for position
    r_if_cond = translate_stata_expression_with_r_values(stata_if_cond, line_num, cmd_df, context)
    # dplyr: filter(condition)
    # collapse: data = data[eval(parse(text=r_if_cond)), ] # More direct but needs care with env
    # Using fsubset from collapse for safety and speed:
    # r_code_str = paste0("data = collapse::fsubset(data, ", r_if_cond, ")") # Recommended
    r_code_str = paste0("data = dplyr::filter(data, ", r_if_cond, ")")
  } else if (is_in_keep) {
    # keep in range
    # Example: keep in 1/10  or  keep in 5  or  keep in f/l
    range_str = stringi::stri_sub(rest_of_cmd_trimmed, from = 4)

    # Parse range: N, N/M, f, l, f/N, N/l
    # Using dplyr::slice()
    # "1/10" -> 1:10
    # "5" -> 5
    # "f" -> 1 (first) - not directly supported by slice range like this, needs min(_n)
    # "l" -> NROW(data) (last) - slice_tail(n=1) or slice(n())
    # For simplicity, assume N/M or N format primarily.
    range_match = stringi::stri_match_first_regex(range_str, "^(\\d+)(?:/(\\d+))?$")
    if (!is.na(range_match[1,1])) {
      start_row = as.integer(range_match[1,2])
      end_row = range_match[1,3]
      if (is.na(end_row)) { # Single number, e.g. keep in 5
        slice_expr = paste0(start_row)
      } else { # Range, e.g. keep in 1/10
        slice_expr = paste0(start_row, ":", as.integer(end_row))
      }
      r_code_str = paste0("data = dplyr::slice(data, ", slice_expr, ")")
    } else {
      # Handle f, l, etc. if needed. For now, basic support.
      # 'f/10' -> 1:10. '5/l' -> 5:NROW(data)
      # This requires more complex parsing.
      r_code_str = paste0("# keep in range '", range_str, "' not fully translated (f/l specifiers).")
    }
  } else {
    # keep varlist
    vars_to_keep = stringi::stri_split_regex(rest_of_cmd_trimmed, "\\s+")[[1]]
    vars_to_keep = vars_to_keep[vars_to_keep != ""]

    if (length(vars_to_keep) == 0) {
      return("# keep command with no variables specified.")
    }
    # dplyr: select(var1, var2, ...)
    # collapse: fselect(data, var1, var2, ...)
    select_vars_r = paste0(vars_to_keep, collapse = ", ")
    r_code_str = paste0("data = dplyr::select(data, ", select_vars_r, ")")
    # Using collapse for potential speed:
    # r_code_str = paste0("data = collapse::fselect(data, ", select_vars_r, ")")
  }

  return(r_code_str)
}

