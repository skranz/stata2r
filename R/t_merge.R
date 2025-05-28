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
  # Corrected regex for merge type to allow 'm'
  # Pattern: ^\s*([1m]:[1m])\s+(.*?)\s+using\s+([^,\s]+)(?:,\\s*(.*))?$
  # G1: type, G2: varlist, G3: filename (can be quoted or macro), G4: options

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
  vars_to_merge_on_r_vec_str = paste0('c("', paste(vars_to_merge_on, collapse = '", "'), '")')

  # Changed default_base_dir_var to "working_dir" for consistency with Stata's default file paths
  using_source_r_expr = resolve_stata_filename(raw_filename_token, cmd_df, line_num, default_base_dir_var = "working_dir")


  # Determine join type based on Stata's `keep()` option or default behavior
  # Stata's default merge behavior is to keep matching observations and unmatched master observations (left_join).
  # If no keep() option is specified, default to left_join.
  join_type_r_func = "dplyr::left_join" # Default for Stata merge
  keep_spec_for_comment = "match master" # Default if no keep() specified

  if (!is.na(options_str)) {
      keep_opt_match = stringi::stri_match_first_regex(options_str, "\\bkeep\\s*\\(([^)]+)\\)")
      if (!is.na(keep_opt_match[1,1])) {
          keep_spec = stringi::stri_trim_both(keep_opt_match[1,2])
          if (grepl("\\ball\\b", keep_spec)) {
              join_type_r_func = "dplyr::full_join"
              keep_spec_for_comment = "all"
          } else if (grepl("\\bmaster\\b", keep_spec)) {
              join_type_r_func = "dplyr::left_join" # Keep matched and master unmatched (left join)
              keep_spec_for_comment = "master"
          } else if (grepl("\\busing\\b", keep_spec)) {
              join_type_r_func = "dplyr::right_join" # Keep matched and using unmatched (right join)
               keep_spec_for_comment = "using"
          } else if (grepl("\\bmatch\\b", keep_spec)) {
              join_type_r_func = "dplyr::inner_join" # Keep matched only (inner join)
              keep_spec_for_comment = "match"
          }
          # Other complex keep() specs like `keep(_merge==3)` are not handled here.
      }
  }

  # Handle nogenerate option - FIX: use regex to correctly detect 'nogen' abbreviation
  has_nogenerate = !is.na(options_str) && stringi::stri_detect_regex(options_str, "\\bno(?:generate|gen)\\b")
  
  # Build the R command string using dplyr::*_join

  # Load the using dataset into a temporary variable first
  r_code_lines = c()
  temp_using_data_var = paste0("stata_tmp_using_data_L", line_num)
  r_code_lines = c(r_code_lines, paste0(temp_using_data_var, " = haven::read_dta(", using_source_r_expr, ")"))

  # Strip haven attributes from both master and using dataframes before joining
  # This can help avoid issues with dplyr operations on labelled columns.
  # Stata merge operates on underlying values, not labels.
  r_code_lines = c(r_code_lines, paste0("data = stata2r::sfun_strip_stata_attributes(data)"))
  r_code_lines = c(r_code_lines, paste0(temp_using_data_var, " = stata2r::sfun_strip_stata_attributes(", temp_using_data_var, ")"))

  # For 1:1 merge, check for unique keys in both master and using datasets to replicate Stata's strictness
  if (merge_type == "1:1") {
      # Use dplyr::select to get the key columns as a tibble before passing to base::duplicated
      r_code_lines = c(r_code_lines,
          paste0("if (any(base::duplicated(dplyr::select(data, dplyr::all_of(", vars_to_merge_on_r_vec_str, "))))) { stop('Merge 1:1 failed: Duplicate keys found in master dataset (data).') }"),
          paste0("if (any(base::duplicated(dplyr::select(", temp_using_data_var, ", dplyr::all_of(", vars_to_merge_on_r_vec_str, "))))) { stop('Merge 1:1 failed: Duplicate keys found in using dataset (', ", using_source_r_expr, ", ').') }")
      )
  }

  # Ensure merge keys are plain numeric for robustness against haven-specific types
  # This section remains, as it explicitly casts to numeric. stripping attributes doesn't guarantee numeric.
  r_code_lines = c(r_code_lines,
      paste0("data = dplyr::mutate(data, ", paste0("`", vars_to_merge_on, "` = as.numeric(`", vars_to_merge_on, "`)", collapse = ", "), ")"),
      paste0(temp_using_data_var, " = dplyr::mutate(", temp_using_data_var, ", ", paste0("`", vars_to_merge_on, "` = as.numeric(`", vars_to_merge_on, "`)", collapse = ", "), ")")
  )


  # Identify common columns that are NOT merge keys
  r_code_lines = c(r_code_lines,
    paste0("common_cols = intersect(names(data), names(", temp_using_data_var, "))"),
    paste0("common_cols_not_by = setdiff(common_cols, ", vars_to_merge_on_r_vec_str, ")")
  )

  # Conditional dropping of columns from the using dataset
  # This is a general Stata merge rule: master's non-key variables take precedence.
  r_code_lines = c(r_code_lines,
    paste0("if (length(common_cols_not_by) > 0) { ", temp_using_data_var, " = dplyr::select(", temp_using_data_var, ", -dplyr::all_of(common_cols_not_by)) }")
  )

  # Perform the join with indicator
  r_code_lines = c(r_code_lines,
    paste0("data = ", join_type_r_func, "(data, ", temp_using_data_var, ", by = ", vars_to_merge_on_r_vec_str, ", indicator = \"", indicator_col_name, "\")")
  )

  # Generate _merge variable unless nogenerate option is present
  if (!has_nogenerate) {
      r_code_lines = c(r_code_lines,
          paste0("data = dplyr::mutate(data, `_merge` = dplyr::case_when("),
          paste0("  `", indicator_col_name, "` == \"left_only\" ~ 1L,"),
          paste0("  `", indicator_col_name, "` == \"right_only\" ~ 2L,"),
          paste0("  `", indicator_col_name, "` == \"both\" ~ 3L,"),
          paste0("  TRUE ~ NA_integer_ # Should not happen if join is successful, but for safety"),
          paste0("))")
      )
  } else {
    r_code_lines = c(r_code_lines, paste0(" # _merge variable was not generated due to 'nogenerate' option."))
  }

  # Always remove the temporary indicator column
  # Use dplyr::any_of to prevent error if column somehow not created (e.g. older dplyr or unexpected join result)
  r_code_lines = c(r_code_lines, paste0("data = dplyr::select(data, -dplyr::any_of('", indicator_col_name, "'))"))

  # Clean up temporary variables
  r_code_lines = c(r_code_lines, paste0("rm(", temp_using_data_var, ", common_cols, common_cols_not_by)"))

  # Add comment about options
  merge_comment_line = paste0("# Stata merge type: ", merge_type, ", keep(", keep_spec_for_comment, ")")
  if (has_nogenerate) {
    merge_comment_line = paste0(merge_comment_line, ", nogenerate")
  }
  r_code_lines = c(r_code_lines, merge_comment_line)

  options_str_cleaned = options_str
  if (!is.na(options_str_cleaned)) {
      # Remove keep() and nogenerate from options string for comment
      options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "\\bkeep\\s*\\([^)]+\\)", "")
      options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "\\bno(?:generate|gen)\\b", "") # Updated to remove both
      options_str_cleaned = stringi::stri_trim_both(stringi::stri_replace_all_regex(options_str_cleaned, ",+", ","))
      options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "^,+", "")
  }
  if (!is.na(options_str_cleaned) && options_str_cleaned != "") {
       r_code_lines = c(r_code_lines, paste0(" # Other options ignored: ", options_str_cleaned))
  }

  return(paste(r_code_lines, collapse="\n"))
}

