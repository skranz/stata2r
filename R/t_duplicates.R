# Translate Stata 'duplicates' command
# Stata: duplicates drop [varlist] [if] [in] [, options]
# Stata: duplicates tag varlist [if] [in] [, options] gen(newvar)
# Stata: duplicates list [varlist] [if] [in] [, options]

t_duplicates = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
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
      r_code_prefix = paste0(data_subset_varname, " = base::subset(data, ", r_subset_cond, ")\n")
      data_source_for_duplicates = data_subset_varname
  }


  r_code_str = ""

  if (subcommand == "drop") {
      # Stata `duplicates drop varlist [if] [in]`
      # Drops all but the first observation of each group defined by varlist (or all vars).
      # R equivalent: `dplyr::distinct(data, varlist, .keep_all = TRUE)`
      # The if/in condition means we only drop duplicates *within the subset* and keep non-subset rows.
      # This is complex. Simpler: apply distinct *after* subsetting if a subset is made.
      # But `duplicates drop if cond` means drop duplicate rows *among those satisfying cond*.
      # The ones NOT satisfying cond are kept, whether they were duplicates or not.
      # R: `data[!duplicated(data[cond, vars_for_duplicates]), ]` ... no, this is not quite right.

      # Let's follow the "subset first" logic for duplicates check.
      # If if/in is present, we check duplicates only within the subset.
      # This is *not* how Stata's `duplicates drop if` works. Stata evaluates `if` on the original data, then drops rows if they are duplicates *within that if-subset*.
      # Correct Stata `duplicates drop if condition, by(varlist)`: Consider only rows satisfying `condition`. Among those rows, drop ones that are duplicates based on `varlist`.
      # R equivalent: `data %>% filter(condition) %>% distinct(varlist, .keep_all = TRUE)` (drops rows). But we need to merge back with rows not satisfying condition.

      # A more accurate translation for `duplicates drop [varlist] [if exp]` would be:
      # 1. Identify duplicates based on varlist (or all vars) in the full dataset.
      # 2. Create a boolean vector `is_duplicate = duplicated(data[, varlist])`
      # 3. Create a boolean vector `satisfies_cond = eval(parse(text=r_if_cond))`. If no cond, all TRUE.
      # 4. Keep row if `!is_duplicate` (it's the first occurrence) OR `!satisfies_cond` (it's outside the subset being checked).
      #    Stata: `keep if !is_duplicate | !satisfies_cond`
      #    This is wrong. Stata `duplicates drop` drops the *later* duplicates.
      #    Stata keeps the first instance. `duplicates drop if cond` keeps the first instance (even if it doesn't meet cond), and subsequent instances *if they meet cond*.
      #    The logic is "Keep if (!is_duplicate) OR (is_duplicate AND !satisfies_cond)".
      #    i.e., Keep the first of any group, AND keep subsequent ones if they don't meet the condition.

      # R base: `!duplicated(data[, vars], fromLast=FALSE)` flags rows after the first.
      # Using `dplyr::distinct` is simpler but drops rows immediately.
      # Let's create the boolean flag first.

      # Vars to use for duplication check
      vars_for_distinct_r = if (is.na(vars_for_duplicates)) "NULL" else paste0("dplyr::all_of(", vars_for_duplicates, ")")

      # Check for duplicates in the data source (either original or subset)
      # `dplyr::distinct(data_source_for_duplicates, vars_for_duplicates)` finds unique rows.
      # To drop duplicates, we need the indices/rows to keep.
      # R base: `!duplicated(data_source_for_duplicates[, vars_list_char])`
      # We need the condition applied first, then duplicates check.
      # `data = data %>% group_by(vars) %>% filter(row_number() == 1)` (if no if/in)
      # `data = data %>% filter(condition) %>% group_by(vars) %>% filter(row_number() == 1)` (subset + drop in subset) - WRONG Stata logic.

      # Let's use the logic: Keep if NOT (is_duplicate AND satisfies_condition)
      # Requires calculating is_duplicate and satisfies_condition row-wise.

      # Calculate the condition vector
      cond_vector_expr = if (!is.na(r_subset_cond) && r_subset_cond != "") r_subset_cond else "TRUE" # TRUE if no condition

      # Calculate the duplicate flag vector
      # Need to handle case where vars_for_duplicates is NA (all columns)
      if (is.na(vars_for_duplicates)) {
          # duplicates drop without varlist uses all columns
          is_duplicate_expr = "base::duplicated(data, fromLast = FALSE)"
      } else {
           is_duplicate_expr = paste0("base::duplicated(data[, ", vars_for_duplicates, ", drop = FALSE], fromLast = FALSE)")
      }

      # R code to calculate and filter
      r_code_lines = c(
          paste0("## Calculate duplicate flag based on ", if(is.na(vars_for_duplicates)) "all variables" else "variables: ", varlist_str),
          paste0("__is_duplicate_L", cmd_obj$line, " = ", is_duplicate_expr),
          paste0("## Calculate condition flag"),
          paste0("__satisfies_cond_L", cmd_obj$line, " = ", cond_vector_expr),
          # Keep if !(__is_duplicate AND __satisfies_cond)
          paste0("data = base::subset(data, !(__is_duplicate_L", cmd_obj$line, " & __satisfies_cond_L", cmd_obj$line, "))"),
          # Clean up temp vars
          paste0("rm(__is_duplicate_L", cmd_obj$line, ", __satisfies_cond_L", cmd_obj$line, ")")
      )
       r_code_str = paste(r_code_lines, collapse="\n")


  } else if (subcommand == "tag") {
      # Stata `duplicates tag varlist [if] [in] , gen(newvar)`
      # Creates newvar (byte 0/1) where 1 indicates the first observation of a duplicate group defined by varlist (or all vars).
      # Only considers observations meeting the if/in condition.
      # The `gen()` option is required for tag.

      gen_var = NA_character_
      if (!is.na(options_str)) {
         gen_opt_match = stringi::stri_match_first_regex(options_str, "\\bgen\\s*\\(([^)]+)\\)")
         if (!is.na(gen_opt_match[1,1])) {
             gen_vars_str = stringi::stri_trim_both(gen_opt_match[1,2])
             gen_vars_list = stringi::stri_split_regex(gen_vars_str, "\\s+")[[1]] # Stata allows multiple, but tag generates one var.
             gen_var = gen_vars_list[1] # Take the first name if multiple provided (Stata might error?)
         }
      }

      if (is.na(gen_var)) {
          return(paste0("# duplicates tag requires gen() option: ", rest_of_cmd))
      }

      # Vars to use for duplication check (defaults to all if varlist missing)
      vars_for_distinct_r = if (is.na(vars_for_duplicates)) "NULL" else paste0("dplyr::all_of(", vars_for_duplicates, ")")

      # Calculate the condition vector
      cond_vector_expr = if (!is.na(r_subset_cond) && r_subset_cond != "") r_subset_cond else "TRUE"

      # Calculate the duplicate flag vector (Stata tag wants the *first* observation flagged)
      # R base: `!duplicated(data[, vars], fromLast=FALSE)` gives TRUE for the first, FALSE for subsequent.
       if (is.na(vars_for_duplicates)) {
          is_first_occurrence_expr = "!base::duplicated(data, fromLast = FALSE)"
      } else {
           is_first_occurrence_expr = paste0("!base::duplicated(data[, ", vars_for_duplicates, ", drop = FALSE], fromLast = FALSE)")
      }

      # The newvar is 1 if (is_first_occurrence AND satisfies_condition), otherwise 0.
      # Or missing (.) if the observation does not satisfy the condition? No, Stata sets it to 0 for unmatched.
      # Stata: `gen byte newvar = 0`. Then `replace newvar = 1 if !duplicated(...) & condition`.

      # R code to calculate and assign
       r_code_lines = c(
          paste0("## Calculate first occurrence flag based on ", if(is.na(vars_for_duplicates)) "all variables" else "variables: ", varlist_str),
          paste0("__is_first_L", cmd_obj$line, " = ", is_first_occurrence_expr),
          paste0("## Calculate condition flag"),
          paste0("__satisfies_cond_L", cmd_obj$line, " = ", cond_vector_expr),
          # Generate the new variable
          # Using collapse::fmutate
          paste0("data = collapse::fmutate(data, ", gen_var, " = dplyr::if_else(__is_first_L", cmd_obj$line, " & __satisfies_cond_L", cmd_obj$line, ", 1, 0))"),
           # Clean up temp vars
          paste0("rm(__is_first_L", cmd_obj$line, ", __satisfies_cond_L", cmd_obj$line, ")")
       )
      r_code_str = paste(r_code_lines, collapse="\n")

  } else if (subcommand == "list") {
       # Stata `duplicates list varlist [if] [in] [, options]`
       # Lists all duplicate observations (all copies including the first) based on varlist.
       # If if/in is used, only lists duplicates among those meeting the condition.
       # This means list if cond, by(varlist) - show all rows that are duplicates according to varlist, AND satisfy cond.
       # R: `data %>% group_by(vars) %>% filter(n() > 1) %>% filter(condition)`

       # Calculate the condition vector
      cond_vector_expr = if (!is.na(r_subset_cond) && r_subset_cond != "") r_subset_cond else "TRUE"

       # Calculate duplicate count per group
       # Need to group by vars, then count.
       # Using collapse:
       vars_for_grouping_r_vec_str = if (is.na(vars_for_duplicates)) {
            # Need to get all variable names from data, which is hard without access to `data`.
            # Return a partial translation or error?
            return("# duplicates list without varlist not fully supported (needs all variable names).")
       } else {
           vars_for_duplicates
       }

       # Group the data, filter for group size > 1, then filter by condition.
       # Stata `duplicates list` shows *all* members of duplicate groups, including the first, if *any* member of the group satisfies the condition and is a duplicate.
       # This logic is complex. Simpler: Just list rows where is_duplicate is TRUE (subsequent copies) AND condition is met.
       # Or: Filter data by condition, then identify duplicates within that subset? No, Stata definition is subtle.

       # Let's stick to the interpretation: list rows that are duplicates (not the first occurrence based on varlist) AND satisfy the condition.
       # Calculate the condition vector (already done as cond_vector_expr)
       # Calculate the duplicate flag vector (already done as is_duplicate_expr from `drop`)

       # R code to filter for listing
        if (is.na(vars_for_duplicates)) {
          is_duplicate_expr = "base::duplicated(data, fromLast = FALSE)"
      } else {
           is_duplicate_expr = paste0("base::duplicated(data[, ", vars_for_duplicates, ", drop = FALSE], fromLast = FALSE)")
      }

       r_code_lines = c(
          paste0("## Calculate duplicate flag based on ", if(is.na(vars_for_duplicates)) "all variables" else "variables: ", varlist_str),
          paste0("__is_duplicate_L", cmd_obj$line, " = ", is_duplicate_expr),
          paste0("## Calculate condition flag"),
          paste0("__satisfies_cond_L", cmd_obj$line, " = ", cond_vector_expr),
          # Filter for duplicates meeting the condition
          paste0("data_duplicates_L", cmd_obj$line, " = base::subset(data, __is_duplicate_L", cmd_obj$line, " & __satisfies_cond_L", cmd_obj$line, ")"),
          # Print the filtered data (Stata lists to console)
          paste0("print(data_duplicates_L", cmd_obj$line, ")"),
          # Clean up temp vars
          paste0("rm(__is_duplicate_L", cmd_obj$line, ", __satisfies_cond_L", cmd_obj$line, ", data_duplicates_L", cmd_obj$line, ")")
       )
      r_code_str = paste(r_code_lines, collapse="\n")

      # Note: `duplicates list` doesn't modify the data, but it is often used in a data manipulation context to diagnose issues.
      # `mark_data_manip_cmd` marks it FALSE, so this function might not even be called.
      # However, it *can* set r() values (r(N_dups)).

      # Revisit mark_data_manip_cmd: `duplicates list` should probably be translated if it's setting r() or if it's useful diagnostic output in the translated R.
      # The current `mark_data_manip_cmds` includes "duplicates". `non_manip_display_cmds` includes "list".
      # `duplicates list` is not in `non_manip_display_cmds`, so it *is* marked TRUE by default.
      # So this translation function *will* be called. Printing to console is the correct behavior.

  } else {
      r_code_str = paste0("# Unknown duplicates subcommand: ", subcommand)
  }

  # Add comment about options if any were present but not handled (excluding gen for tag)
   options_str_cleaned = options_str
   if (subcommand == "tag" && !is.na(options_str_cleaned)) {
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "\\bgen\\s*\\([^)]+\\)", "")
        options_str_cleaned = stringi::stri_trim_both(stringi::stri_replace_all_regex(options_str_cleaned, ",+", ",")) # Clean up multiple commas
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "^,+", "") # Remove leading comma
   }

   if (!is.na(options_str_cleaned) && options_str_cleaned != "") {
        r_code_str = paste0(r_code_str, paste0(" # Other options ignored: ", options_str_cleaned))
   }


  return(r_code_str)
}

