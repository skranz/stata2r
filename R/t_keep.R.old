# Translate Stata 'keep' command
# Can be `keep varlist` or `keep if condition` or `keep in range`

t_keep = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_keep") # Added restore.point
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  is_if_keep = stringi::stri_startswith_fixed(rest_of_cmd_trimmed, "if ")
  is_in_keep = stringi::stri_startswith_fixed(rest_of_cmd_trimmed, "in ")

  r_code_str = "" # Initialize for the different branches

  if (is_if_keep) {
    # keep if condition
    stata_if_cond = stringi::stri_sub(rest_of_cmd_trimmed, from = 4)
    r_if_cond = translate_stata_expression_with_r_values(stata_if_cond, line_num, cmd_df, context)
    # Using dplyr::filter, treating NA in condition as FALSE (Stata behavior)
    r_code_str = paste0("data = dplyr::filter(data, (dplyr::coalesce(as.numeric(", r_if_cond, "), 0) != 0))")
  } else if (is_in_keep) {
    # keep in range
    range_str = stringi::stri_sub(rest_of_cmd_trimmed, from = 4)
    range_match = stringi::stri_match_first_regex(range_str, "^(\\d+)(?:/(\\d+))?$")
    if (!is.na(range_match[1,1])) {
      start_row = as.integer(range_match[1,2])
      end_row = range_match[1,3]
      if (is.na(end_row)) {
        slice_expr = paste0(start_row) # Keep single row
      } else {
        slice_expr = paste0(start_row, ":", as.integer(end_row)) # Keep range
      }
      # Using dplyr::slice
      r_code_str = paste0("data = dplyr::slice(data, ", slice_expr, ")")
    } else {
      r_code_str = paste0("# keep in range '", range_str, "' not fully translated (f/l specifiers).")
    }
  } else {
    # keep varlist
    vars_to_keep = stringi::stri_split_regex(rest_of_cmd_trimmed, "\\s+")[[1]]
    vars_to_keep = vars_to_keep[vars_to_keep != ""]

    if (length(vars_to_keep) == 0) {
      return("# keep command with no variables specified.")
    }
    # Using dplyr::select
    r_code_str = paste0("data = dplyr::select(data, dplyr::all_of(c('", paste(vars_to_keep, collapse="','"), "')))")
  }

  # Update stata2r_original_order_idx to reflect the new row order/count
  if (isTRUE(stata2r_env$has_original_order_idx)) {
    r_code_str = paste0(r_code_str, " %>% \n  dplyr::mutate(stata2r_original_order_idx = dplyr::row_number())")
  }

  return(r_code_str)
}


