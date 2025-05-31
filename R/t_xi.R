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

  # Stata `xi` by default creates dummy variables named `_Ivarname_value`,
  # where `value` is the numeric value of the category, and the first category is omitted.
  # Example: `group_cat` with values 1, 2, 3 -> `_Igroup_cat_2`, `_Igroup_cat_3`.

  # 1. Prepare the source variable for `model.matrix`.
  # `model.matrix` works well with factors. If the Stata variable is numeric with value labels,
  # we need to ensure its numeric values are used as levels for the dummy variable names.
  # So, convert to a factor using the *numeric values* as levels.
  temp_numeric_factor_var = paste0("stata_tmp_xi_numeric_factor_L", line_num, "_", var_to_expand)
  r_code_lines = c(r_code_lines, paste0(temp_numeric_factor_var, " = factor(as.numeric(data[['", var_to_expand, "']]))"))

  # 2. Generate the model matrix. `model.matrix(~ factor_var)` omits the first level by default.
  temp_matrix_var = paste0("stata_tmp_xi_matrix_L", line_num)
  r_code_lines = c(r_code_lines, paste0(temp_matrix_var, " = model.matrix(~ ", temp_numeric_factor_var, ")"))

  # 3. Remove the intercept column, as Stata's `xi` does not generate it.
  r_code_lines = c(r_code_lines, paste0(temp_matrix_var, " = ", temp_matrix_var, "[, !colnames(", temp_matrix_var, ") %in% c(\"(Intercept)\"), drop = FALSE]"))

  # 4. Rename columns to Stata's `_Ivarname_value` format.
  # The column names from `model.matrix` will look like `stata_tmp_xi_numeric_factor_L10_group_cat2` (if 2 is a level).
  # We need to extract the actual numeric level and construct the Stata-style name.
  r_code_lines = c(r_code_lines, paste0("col_names_raw = colnames(", temp_matrix_var, ")"))
  # Extract the last numeric sequence (the category value) from the generated column names.
  r_code_lines = c(r_code_lines, paste0("j_values_from_names = stringi::stri_extract_last_regex(col_names_raw, \"[0-9]+$\")"))
  r_code_lines = c(r_code_lines, paste0("new_col_names_final = paste0(\"_I", var_to_expand, "_\", j_values_from_names)"))
  r_code_lines = c(r_code_lines, paste0("colnames(", temp_matrix_var, ") = new_col_names_final"))

  # 5. Add generated columns to the main data frame.
  # Convert the matrix to a tibble before binding to ensure consistent data types and attributes.
  r_code_lines = c(r_code_lines, paste0("data = dplyr::bind_cols(data, dplyr::as_tibble(", temp_matrix_var, "))"))

  # 6. Set variable labels for the new columns.
  # Stata labels `_Ivarname_value` as `varname==value` (e.g., `group_cat==2`).
  r_code_lines = c(r_code_lines, paste0("for (col_name in new_col_names_final) {"))
  r_code_lines = c(r_code_lines, paste0("  val_part = stringi::stri_replace_first_regex(col_name, \"^_I", var_to_expand, "_\", \"\")"))
  r_code_lines = c(r_code_lines, paste0("  attr(data[[col_name]], \"label\") = paste0(\"", var_to_expand, "==\", val_part)"))
  r_code_lines = c(r_code_lines, paste0("}"))

  # 7. Clean up temporary variables.
  r_code_lines = c(r_code_lines, paste0("rm(", temp_numeric_factor_var, ", ", temp_matrix_var, ", col_names_raw, j_values_from_names, new_col_names_final)"))

  return(paste(r_code_lines, collapse="\n"))
}

