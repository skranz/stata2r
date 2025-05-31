# Translate Stata 'xi' command
# Stata: xi i.varname
# Creates indicator (dummy) variables for a categorical variable.
# By default, it drops the first category as a base.

t_xi = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_xi")
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # For now, focus on the common 'i.varname' syntax for indicator variables.
  # Other `xi` syntax (e.g., `c.varname`, `xi: regress`) are out of scope for this specific fix.
  xi_match = stringi::stri_match_first_regex(rest_of_cmd_trimmed, "^i\\.([a-zA-Z_][a-zA-Z0-9_]*)(?:\\s+(.*))?$")

  if (is.na(xi_match[1,1])) {
    return(paste0("# xi command: Unsupported syntax or variable type for: ", rest_of_cmd, ". Only 'i.varname' is currently supported."))
  }

  var_to_expand = xi_match[1,2] # e.g., "group_cat"
  # Ignore any remaining part for now (e.g., `if` conditions or other variables on the same line).

  r_code_lines = c()

  # 1. Generate the model matrix by converting the variable to a factor first.
  # This ensures `model.matrix` creates dummy variables from numeric or string categories.
  # `haven::as_factor` handles labelled numerics correctly.
  temp_matrix_var = paste0("stata_tmp_xi_matrix_L", line_num)
  r_code_lines = c(r_code_lines, paste0(temp_matrix_var, " = model.matrix(~ haven::as_factor(`", var_to_expand, "`), data = data, na.action = stats::na.pass)"))

  # 2. Remove the intercept column, as Stata's `xi` does not generate it.
  r_code_lines = c(r_code_lines, paste0(temp_matrix_var, " = ", temp_matrix_var, "[, !colnames(", temp_matrix_var, ") %in% c(\"(Intercept)\"), drop = FALSE]"))

  # 3. Rename columns to Stata's `_Ivarname_value` format.
  # The column names from `model.matrix` for `haven::as_factor(var)` are typically `haven::as_factor(var)Level`.
  # We need to extract `Level`.
  r_code_lines = c(r_code_lines, paste0("col_names_raw = colnames(", temp_matrix_var, ")"))
  # Extract the category value by removing the `haven::as_factor(var)` prefix.
  # Note: `model.matrix` does not backtick variable names within the `haven::as_factor()` part of the column name.
  r_code_lines = c(r_code_lines, paste0("j_values_from_names = stringi::stri_replace_first_fixed(col_names_raw, paste0(\"haven::as_factor(\", var_to_expand, \")\"), \"\")"))
  r_code_lines = c(r_code_lines, paste0("new_col_names_final = paste0(\"_I", var_to_expand, "_\", j_values_from_names)"))
  r_code_lines = c(r_code_lines, paste0("colnames(", temp_matrix_var, ") = new_col_names_final"))

  # 4. Add generated columns to the main data frame.
  # Convert the matrix to a tibble before binding to ensure consistent data types and attributes.
  r_code_lines = c(r_code_lines, paste0("data = dplyr::bind_cols(data, dplyr::as_tibble(", temp_matrix_var, "))"))

  # 5. Set variable labels for the new columns.
  # Stata labels `_Ivarname_value` as `varname==value` (e.g., `group_cat==2`).
  r_code_lines = c(r_code_lines, paste0("for (col_name in new_col_names_final) {"))
  r_code_lines = c(r_code_lines, paste0("  val_part = stringi::stri_replace_first_regex(col_name, paste0(\"^_I\", '", var_to_expand, "', \"_\"), \"\")"))
  r_code_lines = c(r_code_lines, paste0("  attr(data[[col_name]], \"label\") = paste0(\"", var_to_expand, "==\", val_part)"))
  r_code_lines = c(r_code_lines, paste0("}"))

  # 6. Clean up temporary variables.
  r_code_lines = c(r_code_lines, paste0("rm(", temp_matrix_var, ", col_names_raw, j_values_from_names, new_col_names_final)"))

  return(paste(r_code_lines, collapse="\n"))
}

