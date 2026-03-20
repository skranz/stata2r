# FILE: R/t_keep.R

# 1. Parsing Phase: Extract Stata syntax components into a structured list
s2r_p_keep = function(rest_of_cmd) {
  restore.point("s2r_p_keep")

  # Leverage the general s2r_parse_if_in helper
  parsed = s2r_parse_if_in(rest_of_cmd)

  res = list(
    varlist = NA_character_,
    if_str = parsed$if_str,
    in_str = parsed$in_str
  )

  # The remaining base string is the varlist for 'keep'
  if (parsed$base_str != "") {
    res$varlist = parsed$base_str
  }

  return(res)
}

# 2. Code Generation Phase: Translate expressions and emit R code
t_keep = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_keep")

  # Parse the Stata command
  parsed = s2r_p_keep(rest_of_cmd)

  # Translate if expression to R
  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) {
    r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, context)
  }

  # Translate in expression to R numeric range
  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  # Build call to runtime execution function `scmd_keep`
  args = c("data = data")
  if (!is.na(parsed$varlist)) {
    args = c(args, paste0("varlist_str = ", quote_for_r_literal(parsed$varlist)))
  }
  if (!is.na(r_if_cond)) {
    args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  }
  if (!is.na(r_in_range)) {
    args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))
  }

  r_code_str = paste0("data = scmd_keep(", paste(args, collapse = ", "), ")")

  # Maintain package internal tracking variables
  if (isTRUE(stata2r_env$has_original_order_idx)) {
    r_code_str = paste0(r_code_str, " %>% \n  dplyr::mutate(stata2r_original_order_idx = dplyr::row_number())")
  }

  return(r_code_str)
}

# 3. Runtime Execution Phase: Evaluate against actual data columns and environments
scmd_keep = function(data, varlist_str = NA_character_, r_if_cond = NA_character_, r_in_range = NA_character_) {
  restore.point("scmd_keep")

  # 1. Row subsetting using generalized if/in evaluator
  data = s2r_eval_if_in(data, r_if_cond, r_in_range)

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

