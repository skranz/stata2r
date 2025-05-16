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
    # Using collapse for potential efficiency
    r_code_str = paste0("data = collapse::fsubset(data, !(", r_if_cond, "))")
    # dplyr alternative:
    # r_code_str = paste0("data = dplyr::filter(data, !(", r_if_cond, "))")
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
      # Using base R for slicing by index as it's direct for negative indices
      r_code_str = paste0("data = data[", slice_expr, ",]")
      # dplyr alternative:
      # r_code_str = paste0("data = dplyr::slice(data, ", slice_expr, ")")
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
    # Using collapse::fdrop
    # fdrop expects unquoted variable names or a character vector with .cols argument
    # To pass as character vector:
    vars_to_drop_r_str = paste0('c("', paste(vars_to_drop, collapse = '","'), '")')
    r_code_str = paste0("data = collapse::fdrop(data, .cols = ",vars_to_drop_r_str, ")")
    # If using bare var names (less safe if var names have spaces or special chars, though Stata usually doesn't)
    # drop_vars_r_fdrop_bare = paste0(vars_to_drop, collapse=", ")
    # r_code_str = paste0("data = collapse::fdrop(data, ", drop_vars_r_fdrop_bare,")")
    # dplyr alternative:
    # r_code_str = paste0("data = dplyr::select(data, -dplyr::any_of(c('", paste(vars_to_drop, collapse="','"), "')))")
  }

  return(r_code_str)
}


