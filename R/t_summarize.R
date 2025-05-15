# Translate Stata 'summarize' or 'su' command
t_summarize = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  # Stata: summarize varlist [if] [in] [weight] [, options]
  # Options: detail, meanonly, format, separator(#), nolabel
  # Stores results in r()
  # r(N), r(mean), r(sd), r(min), r(max), r(sum_w), r(sum)
  # With detail: r(p1), r(p5), ..., r(p99), r(skewness), r(kurtosis), etc.

  # For now, a simplified version focusing on `meanonly` and common stats

  parts = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*([^,]*?)(?:,\\s*(.*))?$")
  varlist_str = stringi::stri_trim_both(parts[1,2])
  options_str = stringi::stri_trim_both(parts[1,3]) # NA if no options

  if (is.na(varlist_str) || varlist_str == "") {
    # summarize without varlist summarizes all variables.
    # This is complex to translate directly to specific r() assignments.
    # For now, comment it out or handle specific cases.
    return("# summarize without varlist not fully supported for r() value generation yet.")
  }

  vars_to_summarize = stringi::stri_split_regex(varlist_str, "\\s+")[[1]]
  vars_to_summarize = vars_to_summarize[vars_to_summarize != ""]

  # If multiple vars, r() typically refers to the first one, unless specific r(varname_stat) exists (not standard)
  # Stata's r(mean) is the mean of the first variable in varlist.
  first_var = vars_to_summarize[1]

  # Check for `meanonly` option
  is_meanonly = !is.na(options_str) && stringi::stri_detect_fixed(options_str, "meanonly")
  is_detail = !is.na(options_str) && stringi::stri_detect_fixed(options_str, "detail")

  r_code_lines = c()

  # Define standard r() results to generate
  # For `summarize varname`: N, mean, sd, min, max
  # For `summarize varname, meanonly`: N, mean
  # For `summarize varname, detail`: many more.

  # Convention for R variable names: stata_r_val_L<line>_<statname>
  # This allows later commands to find them.

  line_prefix = paste0("stata_r_val_L", line_num, "_")

  # Simple case: summarize one variable (or first variable)
  if (is_meanonly) {
    r_code_lines = c(
      r_code_lines,
      # N: count of non-missing observations
      paste0(line_prefix, "N = sum(!is.na(data$", first_var, "))"),
      # mean
      paste0(line_prefix, "mean = mean(data$", first_var, ", na.rm = TRUE)")
    )
  } else { # Default summarize or with other options (detail implies more)
    r_code_lines = c(
      r_code_lines,
      paste0(line_prefix, "N = sum(!is.na(data$", first_var, "))"),
      paste0(line_prefix, "mean = mean(data$", first_var, ", na.rm = TRUE)"),
      paste0(line_prefix, "sd = sd(data$", first_var, ", na.rm = TRUE)"),
      paste0(line_prefix, "min = min(data$", first_var, ", na.rm = TRUE)"),
      paste0(line_prefix, "max = max(data$", first_var, ", na.rm = TRUE)"),
      paste0(line_prefix, "sum = sum(data$", first_var, ", na.rm = TRUE)") # r(sum)
    )
    if (is_detail) {
      # Add more for detail, e.g. percentiles
      # r(p50) is median
      r_code_lines = c(
        r_code_lines,
        paste0(line_prefix, "p50 = median(data$", first_var, ", na.rm = TRUE)")
        # Other percentiles: quantile(data$var, probs = c(0.01, 0.05, ...), na.rm = TRUE)
      )
    }
  }

  # Handle `if` condition if present in varlist_str (e.g. summarize x if y > 0)
  # This is more complex as it means summarizing a subset.
  # For now, assume no `if` condition in `summarize` itself for r() values.
  # Stata: `summarize rate if month==1` uses `if` to filter data before sum.
  # R: `summarize(subset(data, month==1)$rate)`
  # This needs to be parsed from varlist_str if present.
  # A common pattern: `summarize mpg if foreign == 1`
  if_cond_match = stringi::stri_match_first_regex(varlist_str, "\\s+if\\s+(.*)$")
  r_subset_cond = NA_character_

  if(!is.na(if_cond_match[1,1])) {
      stata_if_cond_expr = if_cond_match[1,2]
      varlist_str_cleaned = stringi::stri_replace_all_fixed(varlist_str, if_cond_match[1,1], "") # remove if cond

      # Re-parse vars_to_summarize
      vars_to_summarize = stringi::stri_split_regex(varlist_str_cleaned, "\\s+")[[1]]
      vars_to_summarize = vars_to_summarize[vars_to_summarize != ""]
      first_var = vars_to_summarize[1] # Update first_var

      r_subset_cond = translate_stata_expression_with_r_values(stata_if_cond_expr, line_num, cmd_df, context = list(is_by_group = FALSE))

      # Update r_code_lines to use subset
      # e.g. data$var -> subset(data, r_subset_cond)$var
      # This needs careful formatting.
      # `data_subset_L<line_num> = subset(data, ${r_subset_cond})`
      # Then use `data_subset_L<line_num>$${first_var}`
      data_subset_varname = paste0("data_subset_L", line_num)

      subset_line = paste0(data_subset_varname, " = subset(data, ", r_subset_cond, ")")

      updated_r_code_lines = c()
      for(code_l in r_code_lines){
          updated_r_code_lines = c(updated_r_code_lines,
            stringi::stri_replace_all_fixed(code_l, paste0("data$",first_var), paste0(data_subset_varname,"$",first_var))
          )
      }
      r_code_lines = c(subset_line, updated_r_code_lines)
  }


  if (length(r_code_lines) == 0) {
    return(paste0("# summarize command '", cmd_obj$do_code, "' did not produce specific r() assignments with current logic."))
  }

  return(paste(r_code_lines, collapse="\n"))
}

