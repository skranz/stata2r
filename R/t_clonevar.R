# FILE: R/t_clonevar.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_clonevar = function(rest_of_cmd) {
  restore.point("s2r_p_clonevar")

  parts = stringi::stri_split_fixed(rest_of_cmd, "=", n=2)[[1]]
  if (length(parts) < 2) {
    return(list(new_var = NA_character_, varname = NA_character_, if_str = NA_character_, in_str = NA_character_))
  }

  new_var = stringi::stri_trim_both(parts[1])
  right_side = stringi::stri_trim_both(parts[2])

  parsed = s2r_parse_if_in(right_side)

  varname = stringi::stri_trim_both(parsed$base_str)

  list(
    new_var = new_var,
    varname = varname,
    if_str = parsed$if_str,
    in_str = parsed$in_str
  )
}

# 2. Code Generation Phase: Emit R code
t_clonevar = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_clonevar")
  parsed = s2r_p_clonevar(rest_of_cmd)

  if (is.na(parsed$new_var) || is.na(parsed$varname) || parsed$varname == "") {
    return(paste0("# Failed to parse clonevar command: ", rest_of_cmd))
  }

  r_if_cond = NA_character_
  if (!is.na(parsed$if_str) && parsed$if_str != "") {
    r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, list(is_by_group = FALSE))
  }

  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  args = c("data = data",
           paste0("new_var = ", quote_for_r_literal(parsed$new_var)),
           paste0("varname = ", quote_for_r_literal(parsed$varname)))

  if (!is.na(r_if_cond)) {
    args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  }
  if (!is.na(r_in_range)) {
    args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))
  }

  r_code = paste0("data = scmd_clonevar(", paste(args, collapse = ", "), ")")
  return(r_code)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_clonevar = function(data, new_var, varname, r_if_cond = NA_character_, r_in_range = NA_character_) {
  restore.point("scmd_clonevar")

  var_actual = expand_varlist(varname, names(data))[1]
  if (is.na(var_actual) || !(var_actual %in% names(data))) {
    stop(paste0("scmd_clonevar: variable '", varname, "' not found"))
  }

  n = NROW(data)
  mask = rep(TRUE, n)

  if (!is.na(r_if_cond) && r_if_cond != "") {
    r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))
    mask = mask & s2r_eval_cond(data, r_if_cond, envir = parent.frame())
  }

  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask = rep(FALSE, n)
    in_mask[idx] = TRUE
    mask = mask & in_mask
  }

  src_col = data[[var_actual]]

  # Copy the column and its attributes completely
  new_col = src_col

  if (!all(mask)) {
    # If a subset is specified, non-matching rows become missing
    if (is.character(new_col)) {
      new_col[!mask] = ""
    } else {
      # This handles numeric, integer, factor, and haven_labelled columns
      # while cleanly preserving their label mappings and formats via [<-
      new_col[!mask] = NA
    }
  }

  data[[new_var]] = new_col
  return(data)
}
