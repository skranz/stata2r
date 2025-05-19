# Translate Stata 'order' command
# Stata: order varlist [options]
# Changes the order of variables in the dataset.

t_order = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # Split varlist from options (like `first`, `last`, `after(var)`)
  # Pattern: ^\s*(.*?)(?:,\\s*(.*))?$
  parts = stringi::stri_match_first_regex(rest_of_cmd_trimmed, "^\\s*(.*?)(?:,\\s*(.*))?$")
  varlist_str = stringi::stri_trim_both(parts[1,2])
  options_str = stringi::stri_trim_both(parts[1,3]) # NA if no options

  vars_to_order = stringi::stri_split_regex(varlist_str, "\\s+")[[1]]
  vars_to_order = vars_to_order[vars_to_order != ""]

  if (length(vars_to_order) == 0) {
    return("# order command with no variables specified.")
  }

  # Stata `order varlist` puts varlist at the beginning.
  # Options like `first`, `last`, `after(var)` are not handled here.
  # R equivalent: Select the variables to order, then select all other variables.
  # Using dplyr::select
  # R code: data = dplyr::select(data, var1, var2, ..., dplyr::everything())

  vars_to_order_r_str = paste(vars_to_order, collapse = ", ")

  r_code_str = paste0("data = dplyr::select(data, ", vars_to_order_r_str, ", dplyr::everything())")

  # Add comment about options if any were present but not handled
   if (!is.na(options_str) && options_str != "") {
        r_code_str = paste0(r_code_str, paste0(" # Options ignored: ", options_str))
   }


  return(r_code_str)
}

