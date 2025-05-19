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
  # This regex needs to handle multiple aggregate blocks
  # Find all occurrences of `(stat) var [= newvar]`
  # Let's use stri_match_all_regex
  # Pattern: \( - literal parenthesis
  # ([a-zA-Z_]+) - stat name (group 1)
  # \) - literal parenthesis
  # \s+ - one or more spaces
  # ([a-zA-Z0-9_]+) - source var name (group 2)
  # (?: - non-capturing group for optional assignment
  # \s*=\s* - equals sign with optional spaces
  # ([a-zA-Z0-9_]+) - new var name (group 3)
  # )? - end optional group
  aggregate_matches = stringi::stri_match_all_regex(aggregate_part, "\\(([a-zA-Z_]+)\\)\\s+([a-zA-Z0-9_]+)(?:\\s*=\\s*([a-zA-Z0-9_]+))?")[[1]]
  # aggregate_matches will be a matrix: [match, stat, source_var, new_var]

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
  # Stata collapse applies if/in *before* grouping and aggregation
  r_subset_cond = NA_character_
  data_source_for_collapse = "data"
  r_code_prefix = "" # Code to create subset if needed

  if (!is.na(stata_if_in_cond) && stata_if_in_cond != "") {
      # Assuming if/in condition for collapse applies to original data rows
      # The context for _n/_N here should be the original data, not the group
      r_subset_cond = translate_stata_expression_with_r_values(stata_if_in_cond, line_num, cmd_df, context = list(is_by_group = FALSE))

      # Check if r_subset_cond is valid
      if (is.na(r_subset_cond) || r_subset_cond == "") {
           return(paste0("# Failed to translate if/in condition for collapse: ", stata_if_in_cond))
      }

      # Create a temporary subset variable
      data_subset_varname = paste0("data_subset_L", cmd_obj$line) # Use actual line from cmd_obj
      r_code_prefix = paste0(data_subset_varname, " = base::subset(data, ", r_subset_cond, ")\n")
      data_source_for_collapse = data_subset_varname
  }


  # Build the summarise/aggregate expressions for collapse::fsummarise
  aggregate_exprs = character(NROW(aggregate_matches))
  for (j in 1:NROW(aggregate_matches)) {
    stat = aggregate_matches[j, 2]
    source_var = aggregate_matches[j, 3]
    new_var = aggregate_matches[j, 4]

    # Translate source variable name - usually just the name itself
    r_source_var = translate_stata_expression_with_r_values(source_var, line_num, cmd_df, context)
     if (is.na(r_source_var) || r_source_var == "") {
         return(paste0("# Failed to translate source variable '", source_var, "' for collapse stat '", stat, "'"))
     }


    # Map Stata stats to collapse functions
    collapse_func = switch(stat,
      "mean" = paste0("collapse::fmean(", r_source_var, ", na.rm = TRUE)"),
      "sum" = paste0("collapse::fsum(", r_source_var, ", na.rm = TRUE)"),
      "count" = paste0("collapse::fnobs(", r_source_var, ")"), # counts non-missing
      "N" = paste0("collapse::fnobs(", r_source_var, ")"), # total obs, including missing for the var? No, count() and N() are synonyms, count non-missing.
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
       # Add more stats...
      NULL # Default for unknown stat
    )

    if (is.null(collapse_func)) {
        return(paste0("# Collapse stat '", stat, "' not yet implemented."))
    }

    # Determine the new variable name
    if (is.na(new_var) || new_var == "") {
      # If no newvar specified, Stata uses the source var name.
      # R collapse::fsummarise requires `newvar = func(sourcevar)`.
      # If source_var is `i`, the expression is `i = fmean(i)`.
      r_new_var = r_source_var # Use source_var as the new var name
    } else {
      r_new_var = new_var # Use the specified newvar name
    }

    aggregate_exprs[j] = paste0(r_new_var, " = ", collapse_func)
  }

  # Combine aggregate expressions
  aggregate_exprs_str = paste(aggregate_exprs, collapse = ",\n  ")

  # Build the final R code
  r_code_str = r_code_prefix # Add subsetting code if any

  if (!is.null(by_vars_r_vec_str)) {
    r_code_str = paste0(r_code_str,
                       "data = collapse::fgroup_by(", data_source_for_collapse, ", ", by_vars_r_vec_str, ") %>%\n",
                       "  collapse::fsummarise(", aggregate_exprs_str, ") %>%\n",
                       "  collapse::fungroup()") # Collapse summarise includes ungroup by default, but explicit is safe.
  } else {
     # Collapse without by() aggregates the whole dataset into a single row
     r_code_str = paste0(r_code_str,
                       "data = collapse::fsummarise(", data_source_for_collapse, ", ", aggregate_exprs_str, ")")
  }

  return(r_code_str)
}


