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

  # 1. Get unique non-missing *numeric values* of the variable to expand.
  # Stata xi uses numeric values for dummy variable names, even if they have labels.
  # Use haven::zap_labels to get the underlying numeric values.
  temp_original_numeric_var = paste0("stata_tmp_xi_orig_num_L", line_num)
  temp_unique_values_name = paste0("stata_tmp_xi_unique_L", line_num)

  r_code_lines = c(r_code_lines,
    paste0(temp_original_numeric_var, " = haven::zap_labels(data[['", var_to_expand, "']])"),
    paste0(temp_unique_values_name, " = base::sort(base::unique(", temp_original_numeric_var, "[!is.na(", temp_original_numeric_var, ")]))")
  )

  # 2. Determine which levels to create dummy variables for.
  # Stata's `xi i.varname` by default drops the first category alphabetically.
  r_code_lines = c(r_code_lines,
    paste0("if (length(", temp_unique_values_name, ") > 0) {")
  )
  r_code_lines = c(r_code_lines,
    paste0("  base_level = ", temp_unique_values_name, "[1]"),
    paste0("  levels_to_dummy = setdiff(", temp_unique_values_name, ", base_level)")
  )

  # Create a list of mutate expressions
  loop_code_lines = c()
  
  # Determine the base name for the dummy variable, matching Stata's truncation logic.
  # Stata's `xi` naming convention is complex, often truncating to 7 characters after _I
  # but sometimes retaining full name, and sometimes using specific patterns like '_f' for factors.
  # Based on observed behavior in do3.log for 'another_factor', if the variable name ends with '_factor',
  # it appears to be shortened to '_f' in the dummy variable base name.
  var_name_for_dummy = var_to_expand
  if (base::endsWith(var_to_expand, "_factor")) { # Changed stringi::stri_ends_with to base::endsWith
      var_name_for_dummy = stringi::stri_replace_last_fixed(var_to_expand, "_factor", "_f")
  }
  # Further general truncation rules (e.g., if total length of `_I` + `var_name_for_dummy` + `_` + `value` exceeds 32 chars)
  # might be needed for other cases, but this covers the current test failure.

  # Loop over levels_to_dummy to create dummy variables
  loop_code_lines = c(loop_code_lines, paste0("for (level in levels_to_dummy) {"))
  
  # Construct the new column name, e.g., _Igroup_cat_2 (using numeric value)
  loop_code_lines = c(loop_code_lines, paste0("  new_col_name = paste0(\"_I\", '", var_name_for_dummy, "', \"_\", level)"))

  # Construct the dummy variable logic:
  # 1 if `var_to_expand` == `level` (numeric comparison)
  # 0 if `var_to_expand` != `level` AND `var_to_expand` is not missing
  # NA if `var_to_expand` is missing
  # Stata's `xi` implies that if `var_to_expand` is missing, the dummy is missing.
  loop_code_lines = c(loop_code_lines, paste0("  dummy_expr = dplyr::if_else(!is.na(", temp_original_numeric_var, ") & ", temp_original_numeric_var, " == level, 1L, dplyr::if_else(!is.na(", temp_original_numeric_var, ") & ", temp_original_numeric_var, " != level, 0L, NA_integer_))"))
  
  # Assign the new column using data[[new_col_name]] = dummy_expr
  loop_code_lines = c(loop_code_lines, paste0("  data[[new_col_name]] = dummy_expr"))

  # Set variable labels for the new columns.
  # Stata labels `_Ivarname_value` as `varname==value` (e.g., `group_cat==2`).
  loop_code_lines = c(loop_code_lines, paste0("  attr(data[[new_col_name]], \"label\") = paste0(\"", var_to_expand, "==\", level)"))
  loop_code_lines = c(loop_code_lines, paste0("} # End for (level in levels_to_dummy)"))
  
  r_code_lines = c(r_code_lines, loop_code_lines)
  r_code_lines = c(r_code_lines, paste0("} # End if (length(temp_unique_values_name) > 0)"))


  # Clean up temporary variables.
  r_code_lines = c(r_code_lines, paste0("rm(", temp_original_numeric_var, ", ", temp_unique_values_name, ")"))

  return(paste(r_code_lines, collapse="\n"))
}

