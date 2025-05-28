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

  if (is.na(varlist_str) || varlist_str == "") {
    return("# summarize without varlist not fully supported for r() value generation yet.")
  }

  vars_to_summarize = stringi::stri_split_regex(varlist_str, "\\s+")[[1]]
  vars_to_summarize = vars_to_summarize[vars_to_summarize != ""]
  if (length(vars_to_summarize) == 0) {
      return("# summarize command with no effective variables after parsing conditions.")
  }
  first_var = vars_to_summarize[1] # r() values typically for the first variable

  is_meanonly = !is.na(options_str) && stringi::stri_detect_fixed(options_str, "meanonly")
  is_detail = !is.na(options_str) && stringi::stri_detect_fixed(options_str, "detail")

  r_code_lines = c()
  line_prefix = paste0("stata_r_val_L", cmd_obj$line, "_") # Use cmd_obj$line

  # Prepare data subset if "if condition" is present
  # This creates a temporary subsetted dataframe for summarization if needed.
  # If no if condition, data_source_for_summary refers to the original 'data'.
  data_source_for_summary = "data"
  if (!is.na(stata_if_cond_expr)) {
    r_subset_cond = translate_stata_expression_with_r_values(stata_if_cond_expr, cmd_obj$line, cmd_df, context = list(is_by_group = FALSE)) # Use cmd_obj$line
    data_subset_varname = paste0("data_subset_L", cmd_obj$line) # Use actual line from cmd_obj
    r_code_lines = c(r_code_lines, paste0(data_subset_varname, " = dplyr::filter(data, ", r_subset_cond, ")")) # Changed to dplyr::filter
    data_source_for_summary = data_subset_varname
  }

  # Use base R / dplyr functions for summaries
  if (is_meanonly) {
    r_code_lines = c(
      r_code_lines,
      paste0(line_prefix, "N = NROW(", data_source_for_summary, ")"), # For meanonly, N is total rows
      paste0(line_prefix, "mean = mean(", data_source_for_summary, "[['", first_var, "']], na.rm = TRUE)")
    )
  } else { # Default summarize or with other options (detail implies more)
    r_code_lines = c(
      r_code_lines,
      paste0(line_prefix, "N = NROW(", data_source_for_summary, ")"),
      paste0(line_prefix, "mean = mean(", data_source_for_summary, "[['", first_var, "']], na.rm = TRUE)"),
      paste0(line_prefix, "sd = sd(", data_source_for_summary, "[['", first_var, "']], na.rm = TRUE)"),
      paste0(line_prefix, "min = min(", data_source_for_summary, "[['", first_var, "']], na.rm = TRUE)"),
      paste0(line_prefix, "max = max(", data_source_for_summary, "[['", first_var, "']], na.rm = TRUE)"),
      paste0(line_prefix, "sum = sum(", data_source_for_summary, "[['", first_var, "']], na.rm = TRUE)")
    )
    if (is_detail) {
      r_code_lines = c(
        r_code_lines,
        paste0(line_prefix, "p50 = median(", data_source_for_summary, "[['", first_var, "']], na.rm = TRUE)")
        # Further percentiles: quantile(..., probs = c(0.01, ...), na.rm = TRUE)
        # Stata detail provides: p1, p5, p10, p25, p50, p75, p90, p95, p99
        # Smallest 4, largest 4 values, variance, skewness, kurtosis.
        # This would require more extensive mapping.
      )
    }
  }

  if (length(r_code_lines) == 0) {
    return(paste0("# summarize command '", cmd_obj$do_code, "' did not produce specific r() assignments with current logic."))
  }

  return(paste(r_code_lines, collapse="\n"))
}


