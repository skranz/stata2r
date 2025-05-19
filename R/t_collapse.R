# Translate Stata 'collapse' command
# Stata: collapse (stat) varlist [name=expr ...] [weight] [if] [in] [, options]
# Often: collapse (stat) varlist, by(groupvars)

t_collapse = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {

  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # Split into aggregate definitions part and options part
  parts = stringi::stri_match_first_regex(rest_of_cmd_trimmed, "^\\s*(.*?)(?:,\\s*(.*))?$")
  aggregate_part = stringi::stri_trim_both(parts[1,2])
  options_part = stringi::stri_trim_both(parts[1,3]) # NA if no options

  # Separate potential if/in from aggregate definitions
  stata_if_in_cond = NA_character_
  # Look for `if` or `in` immediately followed by a space
  if_in_match = stringi::stri_match_first_regex(aggregate_part, "\\s+(?:if\\s+|in\\s+)(.*)$")
  if(!is.na(if_in_match[1,1])) {
      stata_if_in_cond = if_in_match[1,2]
      # Remove the if/in part from aggregate_part
      aggregate_part = stringi::stri_replace_last_regex(aggregate_part, "\\s+(?:if\\s+|in\\s+)(.*)$", "")
      aggregate_part = stringi::stri_trim_both(aggregate_part)
  }


  # Parse aggregate definitions: "(stat) var [= newvar] (stat) var [= newvar] ..."
  # Regex: \(stat_word\)\s+(target_var_word)(?:\s*=\s*(source_var_word))?  -- This is for form (stat) target = source
  # OR    \(stat_word\)\s+(source_and_target_var_word)                     -- This is for form (stat) source
  # The current regex is: \\(([a-zA-Z_]+)\\)\\s+([a-zA-Z0-9_]+)(?:\\s*=\s*([a-zA-Z0-9_]+))?
  # G1: stat (stat_from_regex)
  # G2: varname1 (g2_val_from_regex)
  # G3: varname2 (g3_val_from_regex) - optional
  # If G3 is NA: Stata form is (stat) G2. Here, G2 is source and target.
  # If G3 is not NA: Stata form is (stat) G2 = G3. Here, G2 is target, G3 is source.

  aggregate_matches = stringi::stri_match_all_regex(aggregate_part, "\\(([a-zA-Z_]+)\\)\\s+([a-zA-Z0-9_]+)(?:\\s*=\\s*([a-zA-Z0-9_]+))?")[[1]]
  # aggregate_matches will be a matrix: [match, stat, g2_val, g3_val]

  if (NROW(aggregate_matches) == 0) {
    return(paste0("# Failed to parse collapse aggregate definitions: ", aggregate_part))
  }

  # Parse options part for `by()`
  by_vars_collapse = NA_character_
  if (!is.na(options_part)) {
    by_opt_match = stringi::stri_match_first_regex(options_part, "\\bby\\s*\\(([^)]+)\\)")
    if (!is.na(by_opt_match[1,1])) {
      by_vars_collapse = stringi::stri_trim_both(by_opt_match[1,2])
    }
  }

  by_vars_r_vec_str = NULL # For collapse group_by: character vector c("var1", "var2")
  if (!is.na(by_vars_collapse)) {
    by_vars_list = stringi::stri_split_regex(by_vars_collapse, "\\s+")[[1]]
    by_vars_list = by_vars_list[by_vars_list != ""]
    by_vars_r_vec_str = paste0('c("', paste0(by_vars_list, collapse='", "'), '")')
  }

  # Translate the if/in condition for subsetting *before* collapse
  r_subset_cond = NA_character_
  data_source_for_collapse = "data"
  r_code_prefix = "" # Code to create subset if needed

  if (!is.na(stata_if_in_cond) && stata_if_in_cond != "") {
      r_subset_cond = translate_stata_expression_with_r_values(stata_if_in_cond, line_num, cmd_df, context = list(is_by_group = FALSE))
      if (is.na(r_subset_cond) || r_subset_cond == "") {
           return(paste0("# Failed to translate if/in condition for collapse: ", stata_if_in_cond))
      }
      data_subset_varname = paste0("data_subset_L", cmd_obj$line)
      r_code_prefix = paste0(data_subset_varname, " = base::subset(data, ", r_subset_cond, ")\n")
      data_source_for_collapse = data_subset_varname
  }


  # Build the summarise/aggregate expressions for collapse::fsummarise
  aggregate_exprs = character(NROW(aggregate_matches))
  for (j in 1:NROW(aggregate_matches)) {
    stat_from_regex = aggregate_matches[j, 2]
    g2_val_from_regex = aggregate_matches[j, 3]
    g3_val_from_regex = aggregate_matches[j, 4]

    actual_stata_source_var_name = ""
    actual_stata_target_var_name = ""

    if (is.na(g3_val_from_regex)) { # Matched (stat) g2_val_from_regex
        actual_stata_source_var_name = g2_val_from_regex
        actual_stata_target_var_name = g2_val_from_regex
    } else { # Matched (stat) g2_val_from_regex = g3_val_from_regex
        actual_stata_source_var_name = g3_val_from_regex    # G3 is source
        actual_stata_target_var_name = g2_val_from_regex    # G2 is target
    }

    # Translate actual Stata source variable name
    r_source_var = translate_stata_expression_with_r_values(actual_stata_source_var_name, line_num, cmd_df, context)
     if (is.na(r_source_var) || r_source_var == "") {
         return(paste0("# Failed to translate source variable '", actual_stata_source_var_name, "' for collapse stat '", stat_from_regex, "'"))
     }

    # Map Stata stats to collapse functions
    collapse_func = switch(stat_from_regex,
      "mean" = paste0("collapse::fmean(", r_source_var, ", na.rm = TRUE)"),
      "sum" = paste0("collapse::fsum(", r_source_var, ", na.rm = TRUE)"),
      "count" = paste0("collapse::fnobs(", r_source_var, ")"),
      "N" = paste0("collapse::fnobs(", r_source_var, ")"),
      "first" = paste0("collapse::ffirst(", r_source_var, ")"),
      "last" = paste0("collapse::flast(", r_source_var, ")"),
      "min" = paste0("collapse::fmin(", r_source_var, ", na.rm = TRUE)"),
      "max" = paste0("collapse::fmax(", r_source_var, ", na.rm = TRUE)"),
      "median" = paste0("collapse::fmedian(", r_source_var, ", na.rm = TRUE)"),
      "sd" = paste0("collapse::fsd(", r_source_var, ", na.rm = TRUE)"),
      "p1" = paste0("collapse::fquantile(", r_source_var, ", probs = 0.01, na.rm = TRUE)"),
      "p5" = paste0("collapse::fquantile(", r_source_var, ", probs = 0.05, na.rm = TRUE)"),
      "p10" = paste0("collapse::fquantile(", r_source_var, ", probs = 0.10, na.rm = TRUE)"),
      "p25" = paste0("collapse::fquantile(", r_source_var, ", probs = 0.25, na.rm = TRUE)"),
      "p75" = paste0("collapse::fquantile(", r_source_var, ", probs = 0.75, na.rm = TRUE)"),
      "p90" = paste0("collapse::fquantile(", r_source_var, ", probs = 0.90, na.rm = TRUE)"),
      "p95" = paste0("collapse::fquantile(", r_source_var, ", probs = 0.95, na.rm = TRUE)"),
      "p99" = paste0("collapse::fquantile(", r_source_var, ", probs = 0.99, na.rm = TRUE)"),
      NULL
    )

    if (is.null(collapse_func)) {
        return(paste0("# Collapse stat '", stat_from_regex, "' not yet implemented."))
    }

    # The new variable name for R code is the actual_stata_target_var_name
    r_new_var_name = actual_stata_target_var_name
    aggregate_exprs[j] = paste0(r_new_var_name, " = ", collapse_func)
  }

  # Combine aggregate expressions
  aggregate_exprs_str = paste(aggregate_exprs, collapse = ",\n  ")

  # Build the final R code
  r_code_str = r_code_prefix # Add subsetting code if any

  if (!is.null(by_vars_r_vec_str)) {
    r_code_str = paste0(r_code_str,
                       "data = collapse::fgroup_by(", data_source_for_collapse, ", ", by_vars_r_vec_str, ") %>%\n",
                       "  collapse::fsummarise(", aggregate_exprs_str, ") %>%\n",
                       "  collapse::fungroup()")
  } else {
     r_code_str = paste0(r_code_str,
                       "data = collapse::fsummarise(", data_source_for_collapse, ", ", aggregate_exprs_str, ")")
  }

  return(r_code_str)
}

