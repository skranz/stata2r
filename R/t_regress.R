# Translate Stata 'regress' command
# Stata: regress depvar [indepvars] [if] [in] [weight] [, options]
# Primarily for extracting e(sample) if needed by subsequent commands.

t_regress = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_regress")

  # Check if any e() results are actually needed by a subsequent command
  needed_e_results = unlist(cmd_obj$e_results_needed)

  if (length(needed_e_results) == 0) {
    # If no e() results are needed, this regress command doesn't need to produce any R output.
    return(paste0("# regress command at line ", line_num, " translated to no-op as no e() results used later."))
  }

  # Parse `rest_of_cmd` for depvar, indepvars, and if/in conditions
  # Example: "y_outcome x_numeric if some_condition > 10"
  # Regex to capture: (depvar indepvars_optional) (if_clause_optional) (in_clause_optional) (options_like_robust_optional)

  # Remove options like robust, vce(), level() as they don't affect e(sample)
  rest_of_cmd_no_opts = stringi::stri_replace_all_regex(rest_of_cmd, ",\\s*\\w+\\(?[^)]*\\)?", "")
  rest_of_cmd_no_opts = stringi::stri_replace_all_regex(rest_of_cmd_no_opts, ",\\s*robust\\b", "")
  rest_of_cmd_no_opts = stringi::stri_trim_both(rest_of_cmd_no_opts)

  stata_if_cond = NA_character_
  stata_in_range = NA_character_ # Not directly used for e(sample) calculation, but parsed for completeness
  var_part = rest_of_cmd_no_opts

  # Extract `if` condition
  if_match = stringi::stri_match_last_regex(var_part, "\\s+if\\s+(.*)$")
  if (!is.na(if_match[1,1])) {
    stata_if_cond = stringi::stri_trim_both(if_match[1,2])
    var_part = stringi::stri_trim_both(stringi::stri_sub(var_part, 1, if_match[1,1, MRANGE_START=TRUE] - 1))
  }

  vars_str_list = stringi::stri_split_regex(var_part, "\\s+")[[1]]
  vars_str_list = vars_str_list[vars_str_list != ""]

  if (length(vars_str_list) < 1) {
    return(paste0("# regress command at line ", line_num, " has no dependent variable."))
  }
  dep_var = vars_str_list[1]
  indep_vars = if (length(vars_str_list) > 1) vars_str_list[-1] else NULL

  # Construct formula string for R lm (for actual model fitting if needed)
  formula_r_vars = paste0("`", dep_var, "`")
  if (!is.null(indep_vars) && length(indep_vars) > 0) {
    formula_r_vars = paste0(formula_r_vars, " ~ ", paste0("`", indep_vars, "`", collapse = " + "))
  } else {
    formula_r_vars = paste0(formula_r_vars, " ~ 1") # Regress on constant
  }

  all_vars_in_formula = c(dep_var, indep_vars) # All variables involved in the model

  r_code_lines = c()

  # Define the R variable name for e(sample)
  e_sample_r_var_name = paste0("stata_e_sample_L", line_num)
  line_prefix_e_base = paste0("stata_e_L", line_num, "_") # Base prefix for all e() values

  # --- Generate code to calculate e(sample) ---
  # 1. Determine rows satisfying the `if` condition (if any)
  eligible_rows_if_cond_var = paste0("temp_eligible_if_L", line_num)
  if (!is.na(stata_if_cond)) {
    # The 'if' condition for regress is evaluated row-wise on the whole dataset, not per group.
    r_if_cond = translate_stata_expression_with_r_values(stata_if_cond, line_num, cmd_df, context = list(is_by_group = FALSE))
    r_code_lines = c(r_code_lines,
      paste0(eligible_rows_if_cond_var, " = (dplyr::coalesce(as.numeric(with(data, ", r_if_cond, ")), 0) != 0)")
    )
  } else {
    r_code_lines = c(r_code_lines,
      paste0(eligible_rows_if_cond_var, " = rep(TRUE, NROW(data))")
    )
  }

  # 2. Determine rows with complete cases for model variables
  complete_cases_vars_var = paste0("temp_complete_cases_L", line_num)
  vars_for_cc_r_vec = paste0("c('", paste(all_vars_in_formula, collapse="','"), "')")
  r_code_lines = c(r_code_lines,
    paste0(complete_cases_vars_var, " = stats::complete.cases(data[, ", vars_for_cc_r_vec, ", drop=FALSE])")
  )

  # 3. Combine `if` eligibility and complete cases to define e(sample)
  # Stata's `regress` command by default performs listwise deletion. `e(sample)` should reflect this.
  r_code_lines = c(r_code_lines,
    paste0(e_sample_r_var_name, " = as.integer(", eligible_rows_if_cond_var, " & ", complete_cases_vars_var, ")")
  )

  # 4. Clean up temporary logical vectors
  r_code_lines = c(r_code_lines, paste0("rm(", eligible_rows_if_cond_var, ", ", complete_cases_vars_var, ")"))

  # Calculate other e() results if they are needed
  if ("e(N)" %in% needed_e_results) {
      r_code_lines = c(r_code_lines, paste0(line_prefix_e_base, "N = sum(", e_sample_r_var_name, ")"))
  }

  # If other model-derived e() results are needed, run the actual linear model.
  # This avoids running lm if only e(sample) or e(N) are needed.
  other_model_results_needed = setdiff(needed_e_results, c("e(sample)", "e(N)"))
  if (length(other_model_results_needed) > 0) {
    # Filter data to estimation sample before running lm
    lm_data_var = paste0("data_lm_L", line_num)
    r_code_lines = c(r_code_lines,
      paste0(lm_data_var, " = dplyr::filter(data, ", e_sample_r_var_name, " == 1)"),
      paste0("lm_res_L", line_num, " = stats::lm(", formula_r_vars, ", data = ", lm_data_var, ")")
    )
    
    if ("e(r2)" %in% needed_e_results) {
        r_code_lines = c(r_code_lines, paste0(line_prefix_e_base, "r2 = summary(lm_res_L", line_num, ")$r.squared"))
    }
    if ("e(df_r)" %in% needed_e_results) {
        r_code_lines = c(r_code_lines, paste0(line_prefix_e_base, "df_r = lm_res_L", line_num, "$df.residual"))
    }
    if ("e(rmse)" %in% needed_e_results) {
        r_code_lines = c(r_code_lines, paste0(line_prefix_e_base, "rmse = summary(lm_res_L", line_num, ")$sigma"))
    }
    # Clean up lm related temporary variables
    r_code_lines = c(r_code_lines, paste0("rm(", lm_data_var, ", lm_res_L", line_num, ")"))
  }

  # Add a comment about the formula
  r_code_lines = c(r_code_lines, paste0("# Regression model for e() results: ", formula_r_vars))
  if (!is.na(stata_if_cond)) {
    r_code_lines = c(r_code_lines, paste0("# Applied if condition: ", stata_if_cond))
  }

  return(paste(r_code_lines, collapse = "\n"))
}


