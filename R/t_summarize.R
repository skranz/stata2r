# Translate Stata 'summarize' or 'su' command
t_summarize = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_summarize")
  # Stata: summarize varlist [if] [in] [weight] [, options]
  # Options: detail, meanonly, format, separator(#), nolabel
  # Stores results in r()

  # Check if any r() results are actually needed by a subsequent command
  needed_r_results = unlist(cmd_obj$r_results_needed)

  if (length(needed_r_results) == 0) {
    # If no r() results are needed, this summarize command doesn't need to produce any R output.
    return(paste0("# summarize command at line ", line_num, " translated to no-op as no r() results used later."))
  }

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
  } else {
      # If no variables are specified, summarize just counts observations.
      # r(N) is the only relevant r() value.
      if (!("r(N)" %in% needed_r_results)) {
          return(paste0("# summarize command at line ", line_num, " translated to no-op as no r() results used later and no variables specified."))
      }
  }

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

  # Calculate r(N) if needed
  if ("r(N)" %in% needed_r_results) {
      if (is.na(var_for_r_vals)) {
          r_code_lines = c(r_code_lines, paste0(line_prefix, "N = NROW(", data_source_for_summary, ")"))
      } else {
          r_code_lines = c(r_code_lines, paste0(line_prefix, "N = sum(!is.na(", data_source_for_summary, "[['", var_for_r_vals, "']]))"))
      }
  }

  if (!is.na(var_for_r_vals)) {
      if ("r(mean)" %in% needed_r_results) {
          r_code_lines = c(r_code_lines, paste0(line_prefix, "mean = collapse::fmean(", data_source_for_summary, "[['", var_for_r_vals, "']], na.rm = TRUE)"))
      }
      if ("r(sd)" %in% needed_r_results) {
          r_code_lines = c(r_code_lines, paste0(line_prefix, "sd = collapse::fsd(", data_source_for_summary, "[['", var_for_r_vals, "']], na.rm = TRUE)"))
      }
      if ("r(min)" %in% needed_r_results) {
          r_code_lines = c(r_code_lines, paste0(line_prefix, "min = collapse::fmin(", data_source_for_summary, "[['", var_for_r_vals, "']], na.rm = TRUE)"))
      }
      if ("r(max)" %in% needed_r_results) {
          r_code_lines = c(r_code_lines, paste0(line_prefix, "max = collapse::fmax(", data_source_for_summary, "[['", var_for_r_vals, "']], na.rm = TRUE)"))
      }
      if ("r(sum)" %in% needed_r_results) {
          r_code_lines = c(r_code_lines, paste0(line_prefix, "sum = collapse::fsum(", data_source_for_summary, "[['", var_for_r_vals, "']], na.rm = TRUE)"))
      }
      if ("r(p50)" %in% needed_r_results) {
          r_code_lines = c(r_code_lines, paste0(line_prefix, "p50 = collapse::fmedian(", data_source_for_summary, "[['", var_for_r_vals, "']], na.rm = TRUE)"))
      }
  }

  if (data_source_for_summary != "data") {
      r_code_lines = c(r_code_lines, paste0("rm(", data_subset_varname, ")"))
  }

  # Add comment about options (no longer used for logic, but for informative comment)
  options_str_cleaned = options_str
  if (!is.na(options_str_cleaned) && options_str_cleaned != "") {
      r_code_lines = c(r_code_lines, paste0(" # Other options ignored: ", options_str_cleaned))
  }

  if (length(r_code_lines) == 0) {
    return(paste0("# summarize command at line ", line_num, " translated to no-op as no r() results needed for later steps."))
  }

  return(paste(r_code_lines, collapse="\n"))
}


