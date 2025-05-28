# Translate Stata 'collapse' command
# Stata: collapse (stat) varlist [name=expr ...] [weight] [if] [in] [, options]
# Often: collapse (stat) varlist, by(groupvars)

t_collapse = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_collapse")
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # Split into aggregate definitions part and options part
  # Pattern: ^\s*(.*?)(?:,\\s*(.*))?$
  # G1: aggregate_part, G2: options_part
  parts = stringi::stri_match_first_regex(rest_of_cmd_trimmed, "^\\s*(.*?)(?:,\\s*(.*))?$")
  aggregate_part = stringi::stri_trim_both(parts[1,2])
  options_part = stringi::stri_trim_both(parts[1,3]) # NA if no options

  # Separate potential if/in from aggregate definitions
  stata_if_in_cond = NA_character_
  # Look for `if` or `in` immediately followed by a space in the part before the first comma
  if_in_match = stringi::stri_match_first_regex(aggregate_part, "\\s+(?:if\\s+|in\\s+)(.*)$")
  if(!is.na(if_in_match[1,1])) {
      stata_if_in_cond = if_in_match[1,2]
      # Remove the if/in part from aggregate_part
      aggregate_part = stringi::stri_replace_last_regex(aggregate_part, "\\s+(?:if\\s+|in\\s+)(.*)$", "")
      aggregate_part = stringi::stri_trim_both(aggregate_part)
  }


  # Parse aggregate definitions: "(stat) var [name=expr ...] (stat) var [name=expr ...] ..."
  # Updated regex to correctly capture expressions for source and target variables.
  # Group 1: stat name (e.g., mean, sum)
  # Group 2: target variable name (e.g., i, total_i_sum)
  # Group 3: source expression (e.g., i, i+1) - optional, for `name=expr` syntax
  # Changed (?:\\s*=\\s*(.*?))? to (?:\\s*=\\s*([^,]+))? for more robust capture of expressions.
  aggregate_matches = stringi::stri_match_all_regex(aggregate_part, "\\(([a-zA-Z_][a-zA-Z0-9_]*)\\)\\s*([a-zA-Z_][a-zA-Z0-9_.]*)(?:\\s*=\\s*([^,]+))?")[[1]]

  if (NROW(aggregate_matches) == 0) {
    return(paste0("# Failed to parse collapse aggregate definitions: ", aggregate_part))
  }

  # Parse options part for `by()`
  by_vars_list_unquoted = character(0)
  if (!is.na(options_part)) {
    by_opt_match = stringi::stri_match_first_regex(options_part, "\\bby\\s*\\(([^)]+)\\)")
    if (!is.na(by_opt_match[1,1])) {
      by_vars_collapse_str = stringi::stri_trim_both(by_opt_match[1,2])
      by_vars_list_unquoted = stringi::stri_split_regex(by_vars_collapse_str, "\\s+")[[1]]
      by_vars_list_unquoted = by_vars_list_unquoted[by_vars_list_unquoted != ""]
    }
  }

  # Translate the if/in condition for subsetting *before* collapse
  r_code_lines = c()
  data_source_for_collapse = "data"

  if (!is.na(stata_if_in_cond) && stata_if_in_cond != "") {
      r_subset_cond = translate_stata_expression_with_r_values(stata_if_in_cond, cmd_obj$line, cmd_df, context = list(is_by_group = FALSE))
      if (is.na(r_subset_cond) || r_subset_cond == "") {
           return(paste0("# Failed to translate if/in condition for collapse: ", stata_if_in_cond))
      }
      # Using collapse::fsubset. r_subset_cond is a string representing the logical condition.
      r_code_lines = c(r_code_lines, paste0("data = collapse::fsubset(data, ", r_subset_cond, ")"))
      # data_source_for_collapse remains "data" as it's modified in place by fsubset
  }


  # Build the fsummarise expressions
  aggregate_exprs = character(NROW(aggregate_matches))
  new_vars_created = character(NROW(aggregate_matches))
  for (j in 1:NROW(aggregate_matches)) {
    stat_from_regex = aggregate_matches[j, 2] # Group 1: stat name
    actual_stata_target_var_name = stringi::stri_trim_both(aggregate_matches[j, 3]) # Group 2: target var name
    actual_stata_source_expr = stringi::stri_trim_both(aggregate_matches[j, 4]) # Group 3: source expression (optional)

    if (is.na(actual_stata_source_expr) || actual_stata_source_expr == "") {
      # If no explicit source expression (e.g., `(mean) myvar`), the source is the target var itself
      actual_stata_source_expr = actual_stata_target_var_name
    }
    
    new_vars_created[j] = actual_stata_target_var_name

    r_source_expr_translated = translate_stata_expression_with_r_values(actual_stata_source_expr, cmd_obj$line, cmd_df, context)
     if (is.na(r_source_expr_translated) || r_source_expr_translated == "") {
         return(paste0("# Failed to translate source expression '", actual_stata_source_expr, "' for collapse stat '", stat_from_regex, "'"))
     }

    # Map Stata stats to collapse functions
    collapse_func_expr = switch(stat_from_regex,
      "mean" = paste0("collapse::fmean(", r_source_expr_translated, ", na.rm = TRUE)"),
      "sum" = paste0("collapse::fsum(", r_source_expr_translated, ", na.rm = TRUE)"),
      "count" = paste0("collapse::fN(", r_source_expr_translated, ", non.na = TRUE)"), # Counts non-missing values of var/expr
      "N" = "NROW(.)", # N is number of observations in group. NROW(.) in fsummarise.
      "first" = paste0("collapse::ffirst(", r_source_expr_translated, ")"), # na.rm = TRUE by default
      "last" = paste0("collapse::flast(", r_source_expr_translated, ")"),   # na.rm = TRUE by default
      "min" = paste0("collapse::fmin(", r_source_expr_translated, ", na.rm = TRUE)"),
      "max" = paste0("collapse::fmax(", r_source_expr_translated, ", na.rm = TRUE)"),
      "median" = paste0("collapse::fmedian(", r_source_expr_translated, ", na.rm = TRUE)"),
      "sd" = paste0("collapse::fsd(", r_source_expr_translated, ", na.rm = TRUE)"),
      "p1" = paste0("collapse::fquantile(", r_source_expr_translated, ", probs = 0.01, na.rm = TRUE)"),
      "p5" = paste0("collapse::fquantile(", r_source_expr_translated, ", probs = 0.05, na.rm = TRUE)"),
      "p10" = paste0("collapse::fquantile(", r_source_expr_translated, ", probs = 0.10, na.rm = TRUE)"),
      "p25" = paste0("collapse::fquantile(", r_source_expr_translated, ", probs = 0.01, na.rm = TRUE)"),
      "p75" = paste0("collapse::fquantile(", r_source_expr_translated, ", probs = 0.75, na.rm = TRUE)"),
      "p90" = paste0("collapse::fquantile(", r_source_expr_translated, ", probs = 0.90, na.rm = TRUE)"),
      "p95" = paste0("collapse::fquantile(", r_source_expr_translated, ", probs = 0.95, na.rm = TRUE)"),
      "p99" = paste0("collapse::fquantile(", r_source_expr_translated, ", probs = 0.99, na.rm = TRUE)"),
      NULL
    )

    if (is.null(collapse_func_expr)) {
        return(paste0("# Collapse stat '", stat_from_regex, "' not yet implemented for collapse package translation."))
    }

    r_new_var_name = actual_stata_target_var_name
    aggregate_exprs[j] = paste0("`",r_new_var_name, "` = ", collapse_func_expr) # Backticks for safety
  }

  aggregate_exprs_str = paste(aggregate_exprs, collapse = ",\n  ")

  # Build the main data manipulation pipe using collapse
  main_pipe_parts = c("data")
  if (length(by_vars_list_unquoted) > 0) {
    by_vars_fgroup_by_str = paste(by_vars_list_unquoted, collapse = ", ")
    main_pipe_parts = c(main_pipe_parts,
                       paste0("collapse::fgroup_by(", by_vars_fgroup_by_str, ")"))
  }

  main_pipe_parts = c(main_pipe_parts,
                     paste0("collapse::fsummarise(", aggregate_exprs_str, ")"))

  if (length(by_vars_list_unquoted) > 0) {
    main_pipe_parts = c(main_pipe_parts, "collapse::fungroup()")
  }

  # Construct the R code line for data assignment
  # Need to handle if data was already subsetted using `r_code_lines`
  if (length(r_code_lines) > 0) { # This means data = collapse::fsubset(...) was already added
     # The pipe starts from the result of fsubset, which is already assigned to 'data'
     r_code_lines = c(r_code_lines, paste0("data = ", paste(main_pipe_parts, collapse = " %>% \n  ")))
  } else {
     # Pipe starts from original 'data'
     r_code_lines = c(r_code_lines, paste0("data = ", paste(main_pipe_parts, collapse = " %>% \n  ")))
  }


  r_code_str = paste(r_code_lines, collapse="\n")

  # Add comment about options if any were present but not handled (excluding by)
  options_str_cleaned = options_part
  if (!is.na(options_str_cleaned)) {
      options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "\\bby\\s*\\([^)]+\\)", "")
      options_str_cleaned = stringi::stri_trim_both(stringi::stri_replace_all_regex(options_str_cleaned, ",+", ","))
      options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "^,+", "")
      options_str_cleaned = stringi::stri_trim_both(options_str_cleaned)
  }

  if (!is.na(options_str_cleaned) && options_str_cleaned != "") {
       r_code_str = paste0(r_code_str, paste0("\n# Other options ignored: ", options_str_cleaned))
  }

  return(r_code_str)
}


