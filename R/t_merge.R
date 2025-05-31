# Translate Stata 'merge' command
# Stata: merge 1:1 varlist using filename [, options]
# Stata: merge 1:m varlist using filename [, options]
# Stata: merge m:1 varlist using filename [, options]
# Stata: merge m:m varlist using filename [, options]
# Options: keep(match master using all) nogenerate

t_merge = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_merge") # Added restore.point
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # Define a temporary indicator column name for dplyr::join
  indicator_col_name = paste0("stata_merge_indicator_L", line_num)

  # Parse merge type (1:1, 1:m, m:1, m:m), varlist, `using filename`, and options
  merge_match = stringi::stri_match_first_regex(rest_of_cmd_trimmed, "^\\s*([1m]:[1m])\\s+(.*?)\\s+using\\s+(\"[^\"]+\"|`[^']+'|[^,\\s]+)(?:,\\s*(.*))?$")
  merge_type = NA_character_
  varlist_str = NA_character_
  raw_filename_token = NA_character_
  options_str = NA_character_


  if (is.na(merge_match[1,1])) {
      # Check for older syntax without type: `merge varlist using filename` (defaults to 1:1)
      merge_match_old = stringi::stri_match_first_regex(rest_of_cmd_trimmed, "^\\s*(.*?)\\s+using\\s+(\"[^\"]+\"|`[^']+'|[^,\\s]+)(?:,\\s*(.*))?$")
      if (!is.na(merge_match_old[1,1])) {
           merge_type = "1:1" # Assume 1:1 if type not specified
           varlist_str = stringi::stri_trim_both(merge_match_old[1,2])
           raw_filename_token = stringi::stri_trim_both(merge_match_old[1,3])
           options_str = stringi::stri_trim_both(merge_match_old[1,4])
      } else {
           return(paste0("# Failed to parse merge command: ", rest_of_cmd))
      }
  } else {
      merge_type = merge_match[1,2]
      varlist_str = stringi::stri_trim_both(merge_match[1,3])
      raw_filename_token = stringi::stri_trim_both(merge_match[1,4])
      options_str = stringi::stri_trim_both(merge_match[1,5]) # NA if no options
  }

  vars_to_merge_on = stringi::stri_split_regex(varlist_str, "\\s+")[[1]]
  vars_to_merge_on = vars_to_merge_on[vars_to_merge_on != ""]
  if (length(vars_to_merge_on) == 0) {
       return(paste0("# merge command requires varlist: ", rest_of_cmd))
  }
  # For dplyr::join, by argument can be a character vector of column names
  # Create a temporary R variable to hold the character vector of merge keys
  merge_keys_r_var = paste0("stata_merge_keys_L", line_num)
  vars_to_merge_on_r_vec_str = paste0('c("', paste(vars_to_merge_on, collapse = '", "'), '")')


  using_source_r_expr = resolve_stata_filename(raw_filename_token, cmd_df, line_num, default_base_dir_var = "working_dir")

  # Determine if nogenerate option is present (for comments later)
  has_nogenerate = dplyr::coalesce(stringi::stri_detect_regex(options_str, "\\bno(?:generate|gen)\\b"), FALSE)
  
  # Determine keep_spec_for_comment based on parsing `options_str` in `t_merge` scope
  # This variable should be initialized to a default that reflects Stata's behavior for the given merge type.
  initial_keep_spec = NA_character_
  if (merge_type %in% c("1:1", "1:m", "m:1")) {
      initial_keep_spec = "match master"
  } else if (merge_type == "m:m") {
      initial_keep_spec = "match master using"
  }

  keep_spec_for_comment = initial_keep_spec # Default if no keep() specified
  actual_keep_spec_from_options = NA_character_ # What was explicitly written in options_str

  if (!is.na(options_str)) {
      keep_opt_match = stringi::stri_match_first_regex(options_str, "\\bkeep\\s*\\(([^)]+)\\)")
      if (!is.na(keep_opt_match[1,1])) {
          actual_keep_spec_from_options = stringi::stri_trim_both(keep_opt_match[1,2])
          keep_spec_for_comment = actual_keep_spec_from_options # Use actual option for comment
      }
  }

  # --- Start building R code lines ---
  r_code_lines = c()
  temp_using_data_var = paste0("stata_tmp_using_data_L", line_num)

  # Define the merge keys as an R variable
  r_code_lines = c(r_code_lines, paste0(merge_keys_r_var, " = ", vars_to_merge_on_r_vec_str))


  # Read using data
  r_code_lines = c(r_code_lines, paste0(temp_using_data_var, " = haven::read_dta(", using_source_r_expr, ")"))

  # Strip haven attributes from both master and using dataframes before joining
  # And normalize string NAs
  r_code_lines = c(r_code_lines, paste0("data = sfun_strip_stata_attributes(data)"))
  r_code_lines = c(r_code_lines, paste0("data = sfun_normalize_string_nas(data)")) # Added
  r_code_lines = c(r_code_lines, paste0(temp_using_data_var, " = sfun_strip_stata_attributes(", temp_using_data_var, ")"))
  r_code_lines = c(r_code_lines, paste0(temp_using_data_var, " = sfun_normalize_string_nas(", temp_using_data_var, ")")) # Added

  # Ensure merge keys are plain numeric for robustness against haven-specific types
  # Use the merge_keys_r_var for dynamic selection
  r_code_lines = c(r_code_lines,
      paste0("data = dplyr::mutate(data, dplyr::across(dplyr::all_of(", merge_keys_r_var, "), as.numeric))"),
      paste0(temp_using_data_var, " = dplyr::mutate(", temp_using_data_var, ", dplyr::across(dplyr::all_of(", merge_keys_r_var, "), as.numeric))")
  )

  # Logic for 1:1 merge strictness
  if (merge_type == "1:1") {
      r_code_lines = c(r_code_lines,
          paste0("if (any(base::duplicated(dplyr::select(data, dplyr::all_of(", merge_keys_r_var, "))))) { stop('Merge 1:1 failed: Duplicate keys found in master dataset (data).') }"),
          paste0("if (any(base::duplicated(dplyr::select(", temp_using_data_var, ", dplyr::all_of(", merge_keys_r_var, "))))) { stop('Merge 1:1 failed: Duplicate keys found in using dataset (', ", using_source_r_expr, ", ').') }")
      )
  }

  # Identify common columns that are NOT merge keys
  r_code_lines = c(r_code_lines,
    paste0("common_cols = intersect(names(data), names(", temp_using_data_var, "))"),
    paste0("common_cols_not_by = setdiff(common_cols, ", merge_keys_r_var, ")")
  )

  # Conditional dropping of columns from the using dataset
  # Stata's merge logic: if a variable exists in both, the master's version is kept.
  # So, columns in `using` data that conflict with `master` data should be dropped from `using` before join.
  r_code_lines = c(r_code_lines,
    paste0("if (length(common_cols_not_by) > 0) { ", temp_using_data_var, " = dplyr::select(", temp_using_data_var, ", -dplyr::all_of(common_cols_not_by)) }")
  )

  # Always perform a full_join to get the merge indicator. This is the base for subsequent filtering.
  r_code_lines = c(r_code_lines,
    paste0("data = dplyr::full_join(data, ", temp_using_data_var, ", by = ", merge_keys_r_var, ", relationship = \"", merge_type, "\", indicator = \"", indicator_col_name, "\")")
  )

  # Apply Stata's `keep()` logic or defaults by filtering the result of the full_join
  filter_condition_r_expr = NA_character_ # This will hold the R expression for filter
  if (!is.na(actual_keep_spec_from_options)) {
      if (stringi::stri_detect_regex(actual_keep_spec_from_options, "\\ball\\b")) {
          filter_condition_r_expr = "TRUE" # Keep all, no filter needed after full_join
      } else if (stringi::stri_detect_regex(actual_keep_spec_from_options, "\\bmaster\\b")) {
          filter_condition_r_expr = paste0("!!dplyr::sym('", indicator_col_name, "') %in% c(\"left_only\", \"both\")")
      } else if (stringi::stri_detect_regex(actual_keep_spec_from_options, "\\busing\\b")) {
          filter_condition_r_expr = paste0("!!dplyr::sym('", indicator_col_name, "') %in% c(\"right_only\", \"both\")")
      } else if (stringi::stri_detect_regex(actual_keep_spec_from_options, "\\bmatch\\b")) {
          filter_condition_r_expr = paste0("!!dplyr::sym('", indicator_col_name, "') == \"both\"")
      }
  } else { # No explicit keep() option, use Stata defaults
      if (merge_type %in% c("1:1", "1:m", "m:1")) {
          filter_condition_r_expr = paste0("!!dplyr::sym('", indicator_col_name, "') %in% c(\"left_only\", \"both\")") # Stata default is keep(match master)
      } else if (merge_type == "m:m") {
          filter_condition_r_expr = "TRUE" # Stata default is keep(match master using) which is equivalent to keep(all)
      }
  }

  if (!is.na(filter_condition_r_expr) && filter_condition_r_expr != "TRUE") {
      # Use the new expression with !!sym()
      r_code_lines = c(r_code_lines, paste0("data = dplyr::filter(data, ", filter_condition_r_expr, ")"))
  }

  r_code_lines = c(r_code_lines, paste0("data = sfun_normalize_string_nas(data)"))


  # Generate _merge variable unless nogenerate option is present
  if (!has_nogenerate) {
      r_code_lines = c(r_code_lines,
          paste0("data = dplyr::mutate(data, `_merge` = dplyr::case_when("),
          paste0("  !!dplyr::sym('", indicator_col_name, "') == \"left_only\" ~ 1L,"),
          paste0("  !!dplyr::sym('", indicator_col_name, "') == \"right_only\" ~ 2L,"),
          paste0("  !!dplyr::sym('", indicator_col_name, "') == \"both\" ~ 3L,"),
          paste0("  TRUE ~ NA_integer_"), # Fallback for any unexpected values or NAs
          paste0("))")
      )
  } else {
    r_code_lines = c(r_code_lines, paste0(" # _merge variable was not generated due to 'nogenerate' option."))
  }

  # Always remove the temporary indicator column
  r_code_lines = c(r_code_lines, paste0("data = dplyr::select(data, -dplyr::any_of('", indicator_col_name, "'))"))

  # Clean up temporary variables
  r_code_lines = c(r_code_lines, paste0("rm(", temp_using_data_var, ", common_cols, common_cols_not_by, ", merge_keys_r_var, ")"))
  

  # Add comment about options
  merge_comment_line = paste0("# Stata merge type: ", merge_type, ", keep(", keep_spec_for_comment, ")")
  if (has_nogenerate) {
    merge_comment_line = paste0(merge_comment_line, ", nogenerate")
  }
  r_code_lines = c(r_code_lines, merge_comment_line)

  options_str_cleaned = options_str
  if (!is.na(options_str_cleaned)) {
      options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "\\bkeep\\s*\\([^)]+\\)", "")
      options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "\\bno(?:generate|gen)\\b", "")
      options_str_cleaned = stringi::stri_trim_both(stringi::stri_replace_all_regex(options_str_cleaned, ",+", ","))
      options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "^,+", "")
  }
  if (!is.na(options_str_cleaned) && options_str_cleaned != "") {
       r_code_lines = c(r_code_lines, paste0(" # Other options ignored: ", options_str_cleaned))
  }

  return(paste(r_code_lines, collapse="\n"))
}

