# Translate Stata 'summarize' or 'su' command
t_summarize = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_summarize") # Added restore.point
  # Stata: summarize varlist [if] [in] [weight] [, options]
  # Options: detail, meanonly, format, separator(#), nolabel
  # Stores results in r()

  parts = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*([^,]*?)(?:,\\s*(.*))?$")
  varlist_and_cond_str = stringi::stri_trim_both(parts[1,2]) # This may contain "var1 var2 if condition"
  options_str = stringi::stri_trim_both(parts[1,3]) # NA if no options

  # Separate varlist from if condition
  stata_if_cond_expr = NA_character_
  varlist_str = varlist_and_cond_str
  if_cond_match = stringi::stri_match_first_regex(varlist_and_cond_str, "\\s+if\\s+(.*)$")
  if(!is.na(if_cond_match[1,1])) {
      stata_if_cond_expr = if_cond_match[1,2]
      varlist_str = stringi::stri_replace_all_fixed(varlist_and_cond_str, if_cond_match[1,1], "")
      varlist_str = stringi::stri_trim_both(varlist_str)
  }

  vars_to_summarize = stringi::stri_split_regex(varlist_str, "\\s+")[[1]]
  vars_to_summarize = vars_to_summarize[vars_to_summarize != ""]

  # Determine the variable for r() values.
  # If varlist is empty, r(N) is total observations in the sample. Other r() are usually for the first variable.
  # If varlist is present, r(N), r(mean), etc. are for the *last* variable in the varlist.
  var_for_r_vals = NA_character_
  if (length(vars_to_summarize) > 0) {
      var_for_r_vals = vars_to_summarize[length(vars_to_summarize)] # Last variable in varlist
  }

  is_meanonly = dplyr::coalesce(stringi::stri_detect_fixed(options_str, "meanonly"), FALSE)
  is_detail = dplyr::coalesce(stringi::stri_detect_fixed(options_str, "detail"), FALSE)

  r_code_lines = c()
  line_prefix = paste0("stata_r_val_L", cmd_obj$line, "_") # Use cmd_obj$line

  # Prepare data subset if "if condition" is present
  data_source_for_summary = "data"
  if (!is.na(stata_if_cond_expr)) {
    r_subset_cond = translate_stata_expression_with_r_values(stata_if_cond_expr, cmd_obj$line, cmd_df, context = list(is_by_group = FALSE))
    data_subset_varname = paste0("data_subset_L", cmd_obj$line)
    r_code_lines = c(r_code_lines, paste0(data_subset_varname, " = collapse::fsubset(data, (dplyr::coalesce(as.numeric(", r_subset_cond, "), 0) != 0))")) # Used fsubset
    data_source_for_summary = data_subset_varname
  }

  # Calculate r(N)
  # If varlist is empty, r(N) is the number of rows.
  # If varlist is not empty, r(N) is the count of non-missing values for the *last* variable in the varlist.
  if (is.na(var_for_r_vals)) {
      # No varlist specified, r(N) is total observations in the (possibly filtered) dataset
      r_code_lines = c(r_code_lines, paste0(line_prefix, "N = NROW(", data_source_for_summary, ")"))
  } else {
      # Varlist specified, r(N) is non-missing count of the last variable
      r_code_lines = c(r_code_lines, paste0(line_prefix, "N = sum(!is.na(", data_source_for_summary, "[['", var_for_r_vals, "']]))"))
  }


  if (!is.na(var_for_r_vals)) {
      # Use collapse functions for summaries
      # Stata's summarize without detail implies mean, std. dev., min, max, count.
      # meanonly implies only mean.
      if (is_meanonly) {
        r_code_lines = c(
          r_code_lines,
          paste0(line_prefix, "mean = collapse::fmean(", data_source_for_summary, "[['", var_for_r_vals, "']], na.rm = TRUE)")
        )
      } else { # Default summarize or with other options (detail implies more)
        r_code_lines = c(
          r_code_lines,
          paste0(line_prefix, "mean = collapse::fmean(", data_source_for_summary, "[['", var_for_r_vals, "']], na.rm = TRUE)"),
          paste0(line_prefix, "sd = collapse::fsd(", data_source_for_summary, "[['", var_for_r_vals, "']], na.rm = TRUE)"),
          paste0(line_prefix, "min = collapse::fmin(", data_source_for_summary, "[['", var_for_r_vals, "']], na.rm = TRUE)"),
          paste0(line_prefix, "max = collapse::fmax(", data_source_for_summary, "[['", var_for_r_vals, "']], na.rm = TRUE)"),
          paste0(line_prefix, "sum = collapse::fsum(", data_source_for_summary, "[['", var_for_r_vals, "']], na.rm = TRUE)")
        )
        if (is_detail) {
          r_code_lines = c(
            r_code_lines,
            paste0(line_prefix, "p50 = collapse::fmedian(", data_source_for_summary, "[['", var_for_r_vals, "']], na.rm = TRUE)")
            # Further percentiles (p1, p5, etc.), variance, skewness, kurtosis for detail are not yet implemented.
          )
        }
      }
  } else {
      r_code_lines = c(r_code_lines, paste0("# No variable specified for summarize: r(mean), r(sd), etc. not set."))
  }


  if (data_source_for_summary != "data") {
      # Clean up temporary subsetted dataframe
      r_code_lines = c(r_code_lines, paste0("rm(", data_subset_varname, ")"))
  }

  return(paste(r_code_lines, collapse="\n"))
}

