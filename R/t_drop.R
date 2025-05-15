# Translate Stata 'drop' command
# Can be `drop varlist` or `drop if condition` or `drop in range`

t_drop = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {

  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  is_if_drop = stringi::stri_startswith_fixed(rest_of_cmd_trimmed, "if ")
  is_in_drop = stringi::stri_startswith_fixed(rest_of_cmd_trimmed, "in ")

  if (is_if_drop) {
    # drop if condition
    stata_if_cond = stringi::stri_sub(rest_of_cmd_trimmed, from = 4)
    r_if_cond = translate_stata_expression_with_r_values(stata_if_cond, line_num, cmd_df, context)
    # dplyr: filter(!(condition))
    # collapse: fsubset(data, !(condition))
    # r_code_str = paste0("data = collapse::fsubset(data, !(", r_if_cond, "))") # Recommended
    r_code_str = paste0("data = dplyr::filter(data, !(", r_if_cond, "))")
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
      r_code_str = paste0("data = dplyr::slice(data, ", slice_expr, ")")
    } else {
      r_code_str = paste0("# drop in range '", range_str, "' not fully translated (f/l specifiers).")
    }
  } else {
    # drop varlist
    vars_to_drop = stringi::stri_split_regex(rest_of_cmd_trimmed, "\\s+")[[1]]
    vars_to_drop = vars_to_drop[vars_to_drop != ""]

    if (length(vars_to_drop) == 0) {
      return("# drop command with no variables specified.")
    }
    # dplyr: select(-c(var1, var2))
    # collapse: fselect(data, -c(var1, var2)) or fdrop
    # select_vars_r = paste0('"', vars_to_drop, '"', collapse = ", ")
    # r_code_str = paste0("data = dplyr::select(data, -c(", select_vars_r, "))")
    # Using collapse::fselect:
    drop_vars_r_fselect = paste0("-",vars_to_drop, collapse=", ")
    # r_code_str = paste0("data = collapse::fselect(data, ", drop_vars_r_fselect,")")
    # Or dplyr with bare var names:
    drop_vars_r_dplyr = paste0(vars_to_drop, collapse = ", ")
    r_code_str = paste0("data = dplyr::select(data, -dplyr::any_of(c('", paste(vars_to_drop, collapse="','"), "')))")


  }

  return(r_code_str)
}

