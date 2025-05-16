# Translate Stata 'generate' or 'gen' command

# Example Stata: gen newvar = oldvar * 2 if condition
# Example Stata: by group: gen seq = _n
t_generate = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  # Parse `rest_of_cmd` for new variable name, expression, and if condition
  # Example: "newvar = expression [if condition]"

  # Strip type if present (e.g. gen double newvar = ...)
  rest_of_cmd_no_type = stringi::stri_replace_first_regex(rest_of_cmd, "^(?:byte|int|long|float|double)\\s+", "")

  match = stringi::stri_match_first_regex(rest_of_cmd_no_type, "^\\s*([^=\\s]+)\\s*=\\s*(.*?)(?:\\s+if\\s+(.*))?$")

  if (is.na(match[1,1])) {
    return(paste0("# Failed to parse generate command: ", rest_of_cmd))
  }

  new_var = stringi::stri_trim_both(match[1,2])
  stata_expr = stringi::stri_trim_both(match[1,3])
  stata_if_cond = stringi::stri_trim_both(match[1,4]) # Might be NA

  r_expr = translate_stata_expression_with_r_values(stata_expr, line_num, cmd_df, context)

  r_if_cond = NA_character_
  if (!is.na(stata_if_cond) && stata_if_cond != "") {
    r_if_cond = translate_stata_expression_with_r_values(stata_if_cond, line_num, cmd_df, context = list(is_by_group = FALSE))
  }

  by_vars_r_vec_str = NULL
  if (cmd_obj$is_by_prefix && !is.na(cmd_obj$by_vars)) {
    by_vars_list = stringi::stri_split_fixed(cmd_obj$by_vars, " ")[[1]]
    by_vars_list = by_vars_list[by_vars_list != ""] # Remove empty strings if any
    by_vars_r_vec_str = paste0('c("', paste0(by_vars_list, collapse='", "'), '")')
  }

  # Base mutate string
  # For dplyr::if_else, NA needs to be type-specific. This is hard to infer generally.
  # Using NA_real_ as a placeholder. A more robust solution would infer type from r_expr.
  # Base R ifelse is more type-flexible but can have side effects (e.g. de-classing dates).
  if (!is.na(r_if_cond) && r_if_cond != "") {
    # Using base::ifelse for type flexibility of NA, though dplyr::if_else is stricter and often preferred.
    # To use dplyr::if_else, a typed NA (e.g., NA_real_, NA_character_) would be needed for the `false` argument.
    # This example uses `NA_real_`. This might fail if `r_expr` is character.
    # A robust solution might require trying to infer the type of r_expr.
    # For now, let's assume numeric or allow dplyr to potentially error if types mismatch.
    # Using NA which will be logical by default.
    mutate_expr = paste0(new_var, " = dplyr::if_else(", r_if_cond, ", ", r_expr, ", NA_real_)") # Attempt with NA_real_
    # Alternative with base ifelse:
    # mutate_expr = paste0(new_var, " = ifelse(", r_if_cond, ", ", r_expr, ", NA)")

  } else {
    mutate_expr = paste0(new_var, " = ", r_expr)
  }

  if (!is.null(by_vars_r_vec_str)) {
    # Using collapse for grouped mutation:
    r_code_str = paste0("data = collapse::fgroup_by(data, ", by_vars_r_vec_str, ")")
    r_code_str = paste0(r_code_str, "\ndata = collapse::fmutate(data, ", mutate_expr, ")") # fmutate adds or replaces
    r_code_str = paste0(r_code_str, "\ndata = collapse::fungroup(data)")
    # dplyr alternative:
    # by_vars_dplyr_str = gsub('c\\("', '', gsub('"\\)', '', gsub('", "', ',', by_vars_r_vec_str)))
    # r_code_str = paste0("data = data %>%\n  dplyr::group_by(", by_vars_dplyr_str, ") %>%\n  dplyr::mutate(", mutate_expr, ") %>%\n  dplyr::ungroup()")
  } else {
    # Using collapse:
    r_code_str = paste0("data = collapse::fmutate(data, ", mutate_expr, ")")
    # dplyr alternative:
    # r_code_str = paste0("data = dplyr::mutate(data, ", mutate_expr, ")")
  }

  return(r_code_str)
}



