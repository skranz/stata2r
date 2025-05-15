# Translate Stata 'generate' or 'gen' command

# Example Stata: gen newvar = oldvar * 2 if condition
# Example Stata: by group: gen seq = _n
t_generate = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  # Parse `rest_of_cmd` for new variable name, expression, and if condition
  # Example: "newvar = expression [if condition]"
  # A more robust parser is needed for complex cases.
  # Regex for "varname = expression" and optional "if condition"
  # Match: newvar = expr [if cond]
  # Need to handle type: gen byte newvar = ... -> type is for Stata storage, less critical for R type initially.

  # Strip type if present (e.g. gen double newvar = ...)
  rest_of_cmd_no_type = stringi::stri_replace_first_regex(rest_of_cmd, "^(?:byte|int|long|float|double)\\s+", "")

  # Regex to separate varname, expression, and if condition
  # Pattern: `varname = expression [if condition]`
  # `\s*([^=\s]+)\s*=\s*(.*?)(?:\s+if\s+(.*))?$`
  # Group 1: varname
  # Group 2: expression
  # Group 3: if condition (optional)

  match = stringi::stri_match_first_regex(rest_of_cmd_no_type, "^\\s*([^=\\s]+)\\s*=\\s*(.*?)(?:\\s+if\\s+(.*))?$")

  if (is.na(match[1,1])) {
    return(paste0("# Failed to parse generate command: ", rest_of_cmd))
  }

  new_var = stringi::stri_trim_both(match[1,2])
  stata_expr = stringi::stri_trim_both(match[1,3])
  stata_if_cond = stringi::stri_trim_both(match[1,4]) # Might be NA

  # Translate Stata expression and condition to R
  r_expr = translate_stata_expression_with_r_values(stata_expr, line_num, cmd_df, context)

  r_if_cond = NA_character_
  if (!is.na(stata_if_cond) && stata_if_cond != "") {
    r_if_cond = translate_stata_expression_with_r_values(stata_if_cond, line_num, cmd_df, context = list(is_by_group = FALSE)) # if is on whole dataset typically
  }

  # Construct R code (using dplyr or collapse)
  # If 'by' prefix was used (cmd_obj$is_by_prefix and cmd_obj$by_vars)
  by_vars_r = NULL
  if (cmd_obj$is_by_prefix && !is.na(cmd_obj$by_vars)) {
    by_vars_r = paste0("c(", paste0('"', stringi::stri_split_fixed(cmd_obj$by_vars, " ")[[1]], '"', collapse=", "), ")")
  }

  # Base mutate string
  if (!is.na(r_if_cond) && r_if_cond != "") {
    # With if condition: new_var = ifelse(condition, expression, default_value)
    # Default value for gen is missing (NA)
    # Note: Stata's generate with `if` only applies the expression where `if` is true,
    #       otherwise the new variable gets missing for those rows.
    #       dplyr::mutate with if_else achieves this naturally if `false = NA`.
    #       If new_var already exists (should not for gen, but for safety), if_else might be `if_else(cond, expr, existing_var)`
    #       For `gen`, it's always `if_else(cond, expr, NA_type_)` where NA_type depends on expr type.
    #       Safest for `gen` is to ensure it's NA where condition is false or missing.
     mutate_expr = paste0(new_var, " = dplyr::if_else(", r_if_cond, ", ", r_expr, ", NA)") # Ensure NA type matches r_expr
  } else {
    mutate_expr = paste0(new_var, " = ", r_expr)
  }

  if (!is.null(by_vars_r)) {
    # Using collapse for grouped mutation
    # collapse::fmutate by default operates on whole data if `by` not given
    # For by-group: data = fgroup_by(data, group_vars) %>% fmutate(...) %>% fungroup()
    # Or using dplyr: data = data %>% group_by(group_vars) %>% mutate(...) %>% ungroup()
    r_code_str = paste0("data = data %>%\n  dplyr::group_by(", gsub("c\\(|\\)", "", by_vars_r), ") %>%\n  dplyr::mutate(", mutate_expr, ") %>%\n  dplyr::ungroup()")
    # Using collapse:
    # r_code_str = paste0("data = collapse::fmutate(data, ", mutate_expr, ", by = ", by_vars_r, ", add = TRUE)")
    # Check collapse documentation for fmutate with by; `by` is for `collap`, `fgroup_by` is preferred for `fmutate`
    # r_code_str = paste0("data = collapse::fgroup_by(data, ", by_vars_r, ")")
    # r_code_str = paste0(r_code_str, "\ndata = collapse::fmutate(data, ", mutate_expr, ")")
    # r_code_str = paste0(r_code_str, "\ndata = collapse::fungroup(data)")

  } else {
    r_code_str = paste0("data = dplyr::mutate(data, ", mutate_expr, ")")
    # Using collapse:
    # r_code_str = paste0("data = collapse::fmutate(data, ", mutate_expr, ", add = TRUE)")
  }

  return(r_code_str)
}


