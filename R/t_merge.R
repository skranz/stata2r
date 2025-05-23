# Translate Stata 'merge' command
# Stata: merge 1:1 varlist using filename [, options]
# Stata: merge 1:m varlist using filename [, options]
# Stata: merge m:1 varlist using filename [, options]
# Stata: merge m:m varlist using filename [, options]
# Options: keep(match master using all) nogenerate

t_merge = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # Parse merge type (1:1, 1:m, m:1, m:m), varlist, `using filename`, and options
  # Pattern: ^\s*(\d+:\d+)\s+(.*?)\s+using\s+([^,\s]+)(?:,\\s*(.*))?$
  # G1: type, G2: varlist, G3: filename (can be quoted or macro), G4: options

  merge_match = stringi::stri_match_first_regex(rest_of_cmd_trimmed, "^\\s*(\\d+:\\d+)\\s+(.*?)\\s+using\\s+(\"[^\"]+\"|`[^']+'|[^,\\s]+)(?:,\\s*(.*))?$")

  if (is.na(merge_match[1,1])) {
      # Check for older syntax without type: `merge varlist using filename` (defaults to 1:1)
      merge_match_old = stringi::stri_match_first_regex(rest_of_cmd_trimmed, "^\\s*(.*?)\\s+using\\s+(\"[^\"]+\"|`[^']+'|[^,\\s]+)(?:,\\s*(.*))?$")
      if (!is.na(merge_match_old[1,1])) {
           merge_type = "1:1" # Assume 1:1 if type not specified
           varlist_str = stringi::stri_trim_both(merge_match_old[1,2])
           filename_part = stringi::stri_trim_both(merge_match_old[1,3])
           options_str = stringi::stri_trim_both(merge_match_old[1,4])
      } else {
           return(paste0("# Failed to parse merge command: ", rest_of_cmd))
      }
  } else {
      merge_type = merge_match[1,2]
      varlist_str = stringi::stri_trim_both(merge_match[1,3])
      filename_part = stringi::stri_trim_both(merge_match[1,4])
      options_str = stringi::stri_trim_both(merge_match[1,5]) # NA if no options
  }

  vars_to_merge_on = stringi::stri_split_regex(varlist_str, "\\s+")[[1]]
  vars_to_merge_on = vars_to_merge_on[vars_to_merge_on != ""]
  if (length(vars_to_merge_on) == 0) {
       return(paste0("# merge command requires varlist: ", rest_of_cmd))
  }
  vars_to_merge_on_r_vec_str = paste0('c("', paste(vars_to_merge_on, collapse = '", "'), '")')


  # Resolve the `using filename` - can be a path string or a macro
  using_source_r = NA_character_ # This will hold either a path string or an R variable name

  # Check if filename_part is a macro `macroname`
  if (stringi::stri_startswith_fixed(filename_part, "`") && stringi::stri_endswith_fixed(filename_part, "'")) {
    macro_name = stringi::stri_sub(filename_part, 2, -2)

    # Find definition of this macro (from tempfile or save)
    # Convention: R_tempfile_L<def_line>_<macro>_path OR R_tempdata_L<def_line>_<macro>
    path_r_var = NA_character_
    data_r_var = NA_character_

    # Scan cmd_df backwards for tempfile or save definition
    for (i in (line_num - 1):1) {
        if (cmd_df$stata_cmd[i] %in% c("tempfile", "save") && grepl(paste0("`",macro_name,"'"), cmd_df$rest_of_cmd[i])) {
            # Assume tempfile creates R_tempfile_L..._path
            # Assume save `macro` creates R_tempdata_L..._macro
            def_line = cmd_df$line[i]
            path_r_var = paste0("R_tempfile_L", def_line, "_", macro_name, "_path")
            data_r_var = paste0("R_tempdata_L", def_line, "_", macro_name)
            break
        }
    }

    # Prefer using the R data object if it exists, otherwise use the path
    if (!is.na(data_r_var)) {
        using_source_r = data_r_var
        # Need to ensure this variable name is treated as a dataframe in R code
        # e.g., `collapse::fmerge(data, R_tempdata_LXX_macro, by = ...)`
    } else if (!is.na(path_r_var)) {
        # Need to read the DTA file from this path
        # e.g., `collapse::fmerge(data, haven::read_dta(R_tempfile_LXX_macro_path), by = ...)`
        using_source_r = paste0("haven::read_dta(", path_r_var, ")")
    } else {
         # Fallback: assume macro holds a string path or is a string literal to read
         warning(paste0("Macro ",filename_part, " in 'merge' command at line ",line_num, " not fully resolved. Treating as filename string."))
         using_source_r = filename_part
          using_source_r = paste0("haven::read_dta(", using_source_r, ")")
    }
  } else {
    # Actual filename string, e.g. "mydata.dta"
    using_source_r = filename_part
     using_source_r = paste0("haven::read_dta(", using_source_r, ")")
  }


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
  r_code_str = paste0("data = collapse::fmerge(data, ", using_source_r, ", by = ", vars_to_merge_on_r_vec_str, ", all.x = ", toupper(all_x), ", all.y = ", toupper(all_y), ") ", merge_comment)

  return(r_code_str)
}

