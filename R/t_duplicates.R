# Translate Stata 'duplicates' command
# Stata: duplicates drop [varlist] [if] [in] [, options]
# Stata: duplicates tag varlist [if] [in] [, options] gen(newvar)
# Stata: duplicates list [varlist] [if] [in] [, options]

t_duplicates = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_duplicates") # Added restore.point
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # Parse subcommand (drop, tag, list)
  parts_subcmd = stringi::stri_split_regex(rest_of_cmd_trimmed, "\\s+", n=2)[[1]]
  subcommand = stringi::stri_trim_both(parts_subcmd[1])
  rest_after_subcmd = if(length(parts_subcmd) > 1) stringi::stri_trim_both(parts_subcmd[2]) else NA_character_

  if (is.na(subcommand) || !(subcommand %in% c("drop", "tag", "list"))) {
      return(paste0("# Failed to parse duplicates subcommand (drop, tag, or list required): ", rest_of_cmd))
  }

  # Parse varlist, if/in, options from rest_after_subcmd
  # Pattern: `varlist [if] [in] [, options]`
  # This is complex to parse robustly. Let's simplify: assume varlist comes first, then if/in, then options.

  varlist_str = NA_character_
  stata_if_in_cond = NA_character_
  options_str = NA_character_

  # Find if/in first
  if(!is.na(rest_after_subcmd)){ # only parse if rest_after_subcmd is not NA
    if_in_match = stringi::stri_match_first_regex(rest_after_subcmd, "\\s+(?:if\\s+|in\\s+)(.*)$")
    if(!is.na(if_in_match[1,1])) {
        stata_if_in_cond = if_in_match[1,2]
        rest_after_subcmd_no_if_in = stringi::stri_replace_last_regex(rest_after_subcmd, "\\s+(?:if\\s+|in\\s+)(.*)$", "")
        rest_after_subcmd_no_if_in = stringi::stri_trim_both(rest_after_subcmd_no_if_in)
    } else {
        rest_after_subcmd_no_if_in = rest_after_subcmd
    }

     # Find options
    options_match = stringi::stri_match_first_regex(rest_after_subcmd_no_if_in, ",\\s*(.*)$")
    if (!is.na(options_match[1,1])) {
        options_str = stringi::stri_trim_both(options_match[1,2])
        varlist_str = stringi::stri_replace_last_regex(rest_after_subcmd_no_if_in, ",\\s*(.*)$", "")
        varlist_str = stringi::stri_trim_both(varlist_str)
    } else {
        varlist_str = rest_after_subcmd_no_if_in
    }
  }


  # Parse varlist (can be empty, means all variables)
  vars_for_duplicates = NA_character_
  if (!is.na(varlist_str) && varlist_str != "") {
      vars_for_duplicates_list = stringi::stri_split_regex(varlist_str, "\\s+")[[1]]
      vars_for_duplicates_list = vars_for_duplicates_list[vars_for_duplicates_list != ""]
       if (length(vars_for_duplicates_list) > 0) {
           vars_for_duplicates = paste0('c("', paste(vars_for_duplicates_list, collapse = '", "'), '")')
       }
  }


  # Translate the if/in condition for subsetting *before* duplicates logic
  r_subset_cond = NA_character_
  data_source_for_duplicates = "data"
  r_code_prefix = "" # Code to create subset if needed

  if (!is.na(stata_if_in_cond) && stata_if_in_cond != "") {
      # Stata applies if/in condition *before* checking for duplicates.
      # The duplicates check only happens on the subset of data specified by if/in.
      r_subset_cond = translate_stata_expression_with_r_values(stata_if_in_cond, line_num, cmd_df, context = list(is_by_group = FALSE))

      if (is.na(r_subset_cond) || r_subset_cond == "") {
           return(paste0("# Failed to translate if/in condition for duplicates: ", stata_if_in_cond))
      }

      # Create a temporary subset variable
      data_subset_varname = paste0("data_subset_L", cmd_obj$line) # Use actual line from cmd_obj
      r_code_prefix = paste0(data_subset_varname, " = dplyr::filter(data, ", r_subset_cond, ")\n")
      data_source_for_duplicates = data_subset_varname
  }


  r_code_lines = c()

  if (subcommand == "drop") {
      # Calculate the condition vector
      cond_vector_expr = if (!is.na(r_subset_cond) && r_subset_cond != "") r_subset_cond else "TRUE" # TRUE if no condition

      # Calculate the duplicate flag vector
      comment_vars_part = if(is.na(vars_for_duplicates)) "all variables" else paste0("variables: ", varlist_str)
      if (is.na(vars_for_duplicates)) {
          is_duplicate_expr = "base::duplicated(data, fromLast = FALSE)"
      } else {
           is_duplicate_expr = paste0("base::duplicated(data[, ", vars_for_duplicates, ", drop = FALSE], fromLast = FALSE)")
      }

      r_code_lines = c(
          r_code_lines,
          paste0("## Calculate duplicate flag based on ", comment_vars_part),
          paste0("__is_duplicate_L", cmd_obj$line, " = ", is_duplicate_expr),
          paste0("## Calculate condition flag"),
          paste0("__satisfies_cond_L", cmd_obj$line, " = ", cond_vector_expr),
          paste0("data = dplyr::filter(data, !(__is_duplicate_L", cmd_obj$line, " & __satisfies_cond_L", cmd_obj$line, "))"),
          paste0("rm(__is_duplicate_L", cmd_obj$line, ", __satisfies_cond_L", cmd_obj$line, ")")
      )


  } else if (subcommand == "tag") {
      gen_var = NA_character_
      if (!is.na(options_str)) {
         gen_opt_match = stringi::stri_match_first_regex(options_str, "\\bgen\\s*\\(([^)]+)\\)")
         if (!is.na(gen_opt_match[1,1])) {
             gen_vars_str = stringi::stri_trim_both(gen_opt_match[1,2])
             gen_vars_list = stringi::stri_split_regex(gen_vars_str, "\\s+")[[1]]
             gen_var = gen_vars_list[1]
         }
      }

      if (is.na(gen_var)) {
          return(paste0("# duplicates tag requires gen() option: ", rest_of_cmd))
      }

      cond_vector_expr = if (!is.na(r_subset_cond) && r_subset_cond != "") r_subset_cond else "TRUE"
      comment_vars_part = if(is.na(vars_for_duplicates)) "all variables" else paste0("variables: ", varlist_str)

       if (is.na(vars_for_duplicates)) {
          is_first_occurrence_expr = "!base::duplicated(data, fromLast = FALSE)"
      } else {
           is_first_occurrence_expr = paste0("!base::duplicated(data[, ", vars_for_duplicates, ", drop = FALSE], fromLast = FALSE)")
      }

       r_code_lines = c(
          r_code_lines,
          paste0("## Calculate first occurrence flag based on ", comment_vars_part),
          paste0("__is_first_L", cmd_obj$line, " = ", is_first_occurrence_expr),
          paste0("## Calculate condition flag"),
          paste0("__satisfies_cond_L", cmd_obj$line, " = ", cond_vector_expr),
          paste0("data = dplyr::mutate(data, ", gen_var, " = dplyr::if_else(__is_first_L", cmd_obj$line, " & __satisfies_cond_L", cmd_obj$line, ", 1, 0))"),
          paste0("rm(__is_first_L", cmd_obj$line, ", __satisfies_cond_L", cmd_obj$line, ")"),
          # Apply attribute stripping after mutate
          paste0("data$", gen_var, " = sfun_strip_stata_attributes(data$", gen_var, ")")
       )

  } else if (subcommand == "list") {
       cond_vector_expr = if (!is.na(r_subset_cond) && r_subset_cond != "") r_subset_cond else "TRUE"
       comment_vars_part = if(is.na(vars_for_duplicates)) "all variables" else paste0("variables: ", varlist_str)

        if (is.na(vars_for_duplicates)) {
          is_duplicate_expr = "base::duplicated(data, fromLast = FALSE)"
      } else {
           is_duplicate_expr = paste0("base::duplicated(data[, ", vars_for_duplicates, ", drop = FALSE], fromLast = FALSE)")
      }

       r_code_lines = c(
          r_code_lines,
          paste0("## Calculate duplicate flag based on ", comment_vars_part),
          paste0("__is_duplicate_L", cmd_obj$line, " = ", is_duplicate_expr),
          paste0("## Calculate condition flag"),
          paste0("__satisfies_cond_L", cmd_obj$line, " = ", cond_vector_expr),
          paste0("data_duplicates_L", cmd_obj$line, " = dplyr::filter(data, __is_duplicate_L", cmd_obj$line, " & __satisfies_cond_L", cmd_obj$line, ")"),
          paste0("print(data_duplicates_L", cmd_obj$line, ")"),
          paste0("rm(__is_duplicate_L", cmd_obj$line, ", __satisfies_cond_L", cmd_obj$line, ", data_duplicates_L", cmd_obj$line, ")")
       )

  } else {
      r_code_lines = c(r_code_lines, paste0("# Unknown duplicates subcommand: ", subcommand))
  }

  r_code_str = paste(r_code_lines, collapse="\n")

   options_str_cleaned = options_str
   if (subcommand == "tag" && !is.na(options_str_cleaned)) {
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "\\bgen\\s*\\([^)]+\\)", "")
        options_str_cleaned = stringi::stri_trim_both(stringi::stri_replace_all_regex(options_str_cleaned, ",+", ","))
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "^,+", "")
   }

   if (!is.na(options_str_cleaned) && options_str_cleaned != "") {
        r_code_str = paste0(r_code_str, paste0(" # Other options ignored: ", options_str_cleaned))
   }

  return(r_code_str)
}


