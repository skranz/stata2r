# FILE: R/t_order.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_order = function(rest_of_cmd) {
  restore.point("s2r_p_order")
  parts = stringi::stri_match_first_regex(stringi::stri_trim_both(rest_of_cmd), "^\\s*(.*?)(?:,\\s*(.*))?$")
  list(
    varlist = stringi::stri_trim_both(parts[1,2]),
    options = stringi::stri_trim_both(parts[1,3])
  )
}

# 2. Code Generation Phase: Emit R code
t_order = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_order")
  parsed = s2r_p_order(rest_of_cmd)

  if (is.na(parsed$varlist) || parsed$varlist == "") {
    return("# order command with no variables specified.")
  }

  # Build call to runtime execution function `scmd_order`
  args = c("data = data",
           paste0("varlist_str = ", quote_for_r_literal(parsed$varlist)))
  r_code_str = paste0("data = scmd_order(", paste(args, collapse = ", "), ")")

  if (!is.na(parsed$options) && parsed$options != "") {
    r_code_str = paste0(r_code_str, " # Options ignored: ", parsed$options)
  }

  # Maintain package internal tracking variables
  if (isTRUE(stata2r_env$has_original_order_idx)) {
    r_code_str = paste0(r_code_str, " %>% \n  dplyr::mutate(stata2r_original_order_idx = dplyr::row_number())")
  }

  return(r_code_str)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_order = function(data, varlist_str) {
  restore.point("scmd_order")

  # Resolve variable list patterns/abbreviations dynamically
  cols_to_order = expand_varlist(varlist_str, names(data))

  if (length(cols_to_order) > 0) {
    other_cols = setdiff(names(data), cols_to_order)
    data = data[, c(cols_to_order, other_cols), drop = FALSE]
  }

  return(data)
}
