# Translate Stata 'xi' command
# Stata: xi i.varname
# Stata: xi i.varname1*i.varname2
# Creates indicator (dummy) variables for categorical variables and their interactions.
# By default, it drops the first category as a base.

t_xi = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_xi")
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # Regex to capture: i.var1 or i.var1*i.var2
  # G1: var1, G2: var2 (if interaction), G3: remaining options (not used here)
  xi_match = stringi::stri_match_first_regex(rest_of_cmd_trimmed, "^i\\.([a-zA-Z_][a-zA-Z0-9_]*)(?:\\s*\\*\\s*i\\.([a-zA-Z_][a-zA-Z0-9_]*))?(?:\\s+(.*))?$")

  if (is.na(xi_match[1,1])) {
    return(paste0("# xi command: Unsupported syntax or variable type for: ", rest_of_cmd, ". Only 'i.varname' and 'i.varname1*i.varname2' are currently supported."))
  }

  var1_name = xi_match[1,2]
  var2_name = xi_match[1,3] # NA if no interaction

  r_code_lines = c()

  # Helper to generate dummy variables for a single variable, excluding base level
  generate_dummies_for_var = function(varname, original_numeric_var_expr, unique_values_var_expr, base_level_var_expr, levels_to_dummy_var_expr) {
    var_lines = c()
    var_lines = c(var_lines, paste0("  # Dummies for ", varname))
    var_lines = c(var_lines, paste0("  for (level in ", levels_to_dummy_var_expr, ") {"))
    var_lines = c(var_lines, paste0("    new_col_name = paste0(\"_I\", get_xi_base_name(\"", varname, "\"), \"_\", level)"))
    var_lines = c(var_lines, paste0("    dummy_expr = dplyr::if_else(!is.na(", original_numeric_var_expr, ") & ", original_numeric_var_expr, " == level, 1L, dplyr::if_else(!is.na(", original_numeric_var_expr, ") & ", original_numeric_var_expr, " != level, 0L, NA_integer_))"))
    var_lines = c(var_lines, paste0("    data[[new_col_name]] = dummy_expr"))
    var_lines = c(var_lines, paste0("    attr(data[[new_col_name]], \"label\") = paste0(\"", varname, "==\", level)"))
    var_lines = c(var_lines, paste0("  }"))
    return(var_lines)
  }

  # --- Process var1 ---
  temp_orig_num_var1 = paste0("stata_tmp_xi_orig_num_L", line_num, "_", var1_name)
  temp_unique_values_var1 = paste0("stata_tmp_xi_unique_L", line_num, "_", var1_name)
  
  r_code_lines = c(r_code_lines,
    paste0(temp_orig_num_var1, " = haven::zap_labels(data[['", var1_name, "']])"),
    paste0(temp_unique_values_var1, " = base::sort(base::unique(", temp_orig_num_var1, "[!is.na(", temp_orig_num_var1, ")]))")
  )
  
  r_code_lines = c(r_code_lines, paste0("if (length(", temp_unique_values_var1, ") > 0) {"))
  r_code_lines = c(r_code_lines, paste0("  base_level_", var1_name, " = ", temp_unique_values_var1, "[1]"))
  r_code_lines = c(r_code_lines, paste0("  levels_to_dummy_", var1_name, " = setdiff(", temp_unique_values_var1, ", base_level_", var1_name, ")"))

  # Generate main effect dummies for var1
  r_code_lines = c(r_code_lines, generate_dummies_for_var(var1_name, temp_orig_num_var1, temp_unique_values_var1, paste0("base_level_", var1_name), paste0("levels_to_dummy_", var1_name)))

  # --- Process var2 if interaction exists ---
  if (!is.na(var2_name)) {
    temp_orig_num_var2 = paste0("stata_tmp_xi_orig_num_L", line_num, "_", var2_name)
    temp_unique_values_var2 = paste0("stata_tmp_xi_unique_L", line_num, "_", var2_name)

    r_code_lines = c(r_code_lines,
      paste0("  ", temp_orig_num_var2, " = haven::zap_labels(data[['", var2_name, "']])"),
      paste0("  ", temp_unique_values_var2, " = base::sort(base::unique(", temp_orig_num_var2, "[!is.na(", temp_orig_num_var2, ")]))")
    )

    r_code_lines = c(r_code_lines, paste0("  if (length(", temp_unique_values_var2, ") > 0) {"))
    r_code_lines = c(r_code_lines, paste0("    base_level_", var2_name, " = ", temp_unique_values_var2, "[1]"))
    r_code_lines = c(r_code_lines, paste0("    levels_to_dummy_", var2_name, " = setdiff(", temp_unique_values_var2, ", base_level_", var2_name, ")"))

    # Generate main effect dummies for var2
    r_code_lines = c(r_code_lines, generate_dummies_for_var(var2_name, temp_orig_num_var2, temp_unique_values_var2, paste0("base_level_", var2_name), paste0("levels_to_dummy_", var2_name)))

    # Generate interaction dummies
    r_code_lines = c(r_code_lines, paste0("    # Interaction effects for ", var1_name, "*", var2_name))
    r_code_lines = c(r_code_lines, paste0("    interaction_base = get_xi_interaction_basename(\"", var1_name, "\", \"", var2_name, "\")"))
    r_code_lines = c(r_code_lines, paste0("    for (level1 in levels_to_dummy_", var1_name, ") {"))
    r_code_lines = c(r_code_lines, paste0("      for (level2 in levels_to_dummy_", var2_name, ") {"))
    r_code_lines = c(r_code_lines, paste0("        new_col_name = paste0(\"_I\", interaction_base, \"_\", level1, \"_\", level2)"))
    r_code_lines = c(r_code_lines, paste0("        dummy_expr = dplyr::if_else("))
    r_code_lines = c(r_code_lines, paste0("          !is.na(", temp_orig_num_var1, ") & !is.na(", temp_orig_num_var2, ") & ", temp_orig_num_var1, " == level1 & ", temp_orig_num_var2, " == level2, 1L,"))
    r_code_lines = c(r_code_lines, paste0("          dplyr::if_else("))
    r_code_lines = c(r_code_lines, paste0("            !is.na(", temp_orig_num_var1, ") & !is.na(", temp_orig_num_var2, ") & (", temp_orig_num_var1, " != level1 | ", temp_orig_num_var2, " != level2), 0L,"))
    r_code_lines = c(r_code_lines, paste0("            NA_integer_"))
    r_code_lines = c(r_code_lines, paste0("          )"))
    r_code_lines = c(r_code_lines, paste0("        )"))
    r_code_lines = c(r_code_lines, paste0("        data[[new_col_name]] = dummy_expr"))
    r_code_lines = c(r_code_lines, paste0("        attr(data[[new_col_name]], \"label\") = paste0(\"", var1_name, "==\", level1, \" & \", \"", var2_name, "==\", level2)"))
    r_code_lines = c(r_code_lines, paste0("      }"))
    r_code_lines = c(r_code_lines, paste0("    }"))
    r_code_lines = c(r_code_lines, paste0("  } # End if (length(temp_unique_values_var2) > 0)"))
    r_code_lines = c(r_code_lines, paste0("  rm(", temp_orig_num_var2, ", ", temp_unique_values_var2, ")"))
  }
  
  r_code_lines = c(r_code_lines, paste0("} # End if (length(temp_unique_values_var1) > 0)"))
  r_code_lines = c(r_code_lines, paste0("rm(", temp_orig_num_var1, ", ", temp_unique_values_var1, ")"))

  return(paste(r_code_lines, collapse="\n"))
}


