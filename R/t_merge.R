# Translate Stata 'merge' command
# Stata: merge 1:1 varlist using filename [, options]
# Stata: merge 1:m varlist using filename [, options]
# Stata: merge m:1 varlist using filename [, options]
# Stata: merge m:m varlist using filename [, options]
# Options: keep(match master using all) nogenerate

t_merge = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_merge") # Added restore.point
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

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
  vars_to_merge_on_r_vec_str = paste0('c("', paste(vars_to_merge_on, collapse = '", "'), '")')

  using_source_r_expr = resolve_stata_filename(raw_filename_token, cmd_df, line_num, default_base_dir_var = "data_dir")


  # Determine merge type for collapse::fmerge
  # Stata default for all merge types is to drop observations that do not match in both.
  # So R inner_join (all.x=F, all.y=F) is the closest default.
  all_x = FALSE # Default to inner join behavior (Stata's default)
  all_y = FALSE # Default to inner join behavior (Stata's default)
  merge_comment = paste0("# Stata merge type: ", merge_type, ", default: keep(match)")

  # Handle keep() options if present, overriding defaults
  # This is a simplified parser for keep options within merge
  if (!is.na(options_str)) {
      keep_opt_match = stringi::stri_match_first_regex(options_str, "\\bkeep\\s*\\(([^)]+)\\)")
      if (!is.na(keep_opt_match[1,1])) {
          keep_spec = stringi::stri_trim_both(keep_opt_match[1,2])
          if (grepl("\\ball\\b", keep_spec)) {
              all_x = TRUE; all_y = TRUE
              merge_comment = paste0(merge_comment, ", keep(all)")
          } else if (grepl("\\bmaster\\b", keep_spec)) {
              all_x = TRUE; all_y = FALSE # Keep matched and master unmatched (left join)
              merge_comment = paste0(merge_comment, ", keep(master)")
          } else if (grepl("\\busing\\b", keep_spec)) {
              all_x = FALSE; all_y = TRUE # Keep matched and using unmatched (right join)
               merge_comment = paste0(merge_comment, ", keep(using)")
          } else if (grepl("\\bmatch\\b", keep_spec)) {
              all_x = FALSE; all_y = FALSE # Keep matched only (inner join)
              merge_comment = paste0(merge_comment, ", keep(match)")
          }
          # Other complex keep() specs like `keep(_merge==3)` are not handled here.
      }
  }

  # Handle nogenerate option
  has_nogenerate = !is.na(options_str) && stringi::stri_detect_fixed(options_str, "nogenerate")
  # Stata merge creates _merge variable (1 master, 2 using, 3 both).
  # R merge/join doesn't create a merge indicator. `collapse::fmerge` also doesn't.
  # If nogenerate is NOT present, we would need to create a merge indicator variable.
  # This is complex. For now, assume nogenerate or don't generate the indicator.
  # Add a comment if _merge variable is expected but not generated.
  merge_comment = paste0(merge_comment, if(has_nogenerate) ", nogenerate" else " # _merge variable was not generated.")


  # Build the R command string using collapse::fmerge
  # fmerge(x, y, by, all.x, all.y)
  r_code_str = paste0("data = collapse::fmerge(data, haven::read_dta(", using_source_r_expr, "), by = ", vars_to_merge_on_r_vec_str, ", all.x = ", toupper(all_x), ", all.y = ", toupper(all_y), ") ", merge_comment)

  return(r_code_str)
}

