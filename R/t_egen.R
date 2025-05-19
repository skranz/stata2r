# Translate Stata 'egen' command
# Stata: egen [type] newvar = fcn(arguments) [if exp] [in range] [, options]

# This is a complex command with many functions.
# We'll implement a few common ones like mean, total, rank.
t_egen = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  # Basic parsing: newvar = function(args) [, by(groupvars)] [if condition]
  # Example: egen mean_i_grp = mean(i), by(group)
  # Example: egen total_i = total(i)
  # Example: bysort group: egen rank_i = rank(i) (Note: bysort handled by cmd_obj$is_by_prefix)

  # Remove type prefix if any (byte, int, long, float, double, str#, etc.)
  # Pattern: ^\s*(byte|int|long|float|double|str\d+)\s+
  rest_of_cmd_no_type = stringi::stri_replace_first_regex(rest_of_cmd, "^\\s*(?:byte|int|long|float|double|str\\d+)\\s+", "")

  # Re-parse rest_of_cmd_no_type looking for `newvar = fcn(args) [if cond] [, options]`
  # Split at the first `=`. Left is `newvar`. Right is `fcn(args) [if cond] [, options]`
  parts_eq = stringi::stri_split_fixed(rest_of_cmd_no_type, "=", n=2)[[1]]
  if(length(parts_eq) != 2) return(paste0("# Failed to parse egen command structure (no =): ", rest_of_cmd))

  new_var = stringi::stri_trim_both(parts_eq[1])
  right_part = stringi::stri_trim_both(parts_eq[2])

  # Split right_part at the first comma (if any) to separate function/args/if from options
  parts_comma = stringi::stri_split_fixed(right_part, ",", n=2)[[1]]
  func_args_if_part = stringi::stri_trim_both(parts_comma[1])
  options_str = if(length(parts_comma) > 1) stringi::stri_trim_both(parts_comma[2]) else NA_character_

  # Now parse func_args_if_part: `fcn(args) [if cond]`
  # Split at the first `(`
  parts_paren = stringi::stri_split_fixed(func_args_if_part, "(", n=2)[[1]]
  if(length(parts_paren) != 2) return(paste0("# Failed to parse egen function call: ", func_args_if_part))

  egen_func_name = stringi::stri_trim_both(parts_paren[1])
  args_and_if_part = stringi::stri_trim_both(stringi::stri_replace_last_fixed(parts_paren[2], ")", "")) # Remove trailing ')'

  # Now parse args_and_if_part: `args [if cond]` or `args [in range]`
  stata_if_cond_in_args = NA_character_
  stata_in_range_in_args = NA_character_
  egen_args_str = args_and_if_part

  # Look for `if` or `in` within the args part
  # Check for `if` first
  if_match_in_args = stringi::stri_match_first_regex(egen_args_str, "\\s+if\\s+(.*)$")
   if(!is.na(if_match_in_args[1,1])) {
      stata_if_cond_in_args = if_match_in_args[1,2]
      egen_args_str = stringi::stri_replace_last_regex(egen_args_str, "\\s+if\\s+(.*)$", "")
      egen_args_str = stringi::stri_trim_both(egen_args_str)
   }

  # Check for `in`
  in_match_in_args = stringi::stri_match_first_regex(egen_args_str, "\\s+in\\s+(.*)$")
   if(!is.na(in_match_in_args[1,1])) {
      stata_in_range_in_args = in_match_in_args[1,2]
      egen_args_str = stringi::stri_replace_last_regex(egen_args_str, "\\s+in\\s+(.*)$", "")
      egen_args_str = stringi::stri_trim_both(egen_args_str)
   }

  # Now we have: new_var, egen_func_name, egen_args_str, stata_if_cond_in_args, stata_in_range_in_args, options_str

  # Translate the condition/range if it exists
  r_if_cond_in_args = NA_character_
  if (!is.na(stata_if_cond_in_args) && stata_if_cond_in_args != "") {
       # Context for _n/_N in the if condition within egen args is usually the group context (if by_prefix used)
      r_if_cond_in_args = translate_stata_expression_with_r_values(stata_if_cond_in_args, line_num, cmd_df, context)
       if (is.na(r_if_cond_in_args) || r_if_cond_in_args == "") {
           return(paste0("# Failed to translate if condition in egen args: ", stata_if_cond_in_args))
       }
  }

   r_in_range_cond_in_args = NA_character_
  if (!is.na(stata_in_range_in_args) && stata_in_range_in_args != "") {
       # Context for _n/_N etc. in range is group context if by_prefix used.
       # Stata `in f/l` in egen refers to observation numbers *within the group* if bysort prefix is used.
       # Otherwise, it refers to global observation numbers.
       # The `context$is_by_group` flag from parse_stata_command_line indicates bysort prefix.
       context_for_range = list(is_by_group = cmd_obj$is_by_prefix)

       range_match = stringi::stri_match_first_regex(stata_in_range_in_args, "^(\\d+)(?:/(\\d+))?$")
        if (!is.na(range_match[1,1])) {
            start_row = as.integer(range_match[1,2])
            end_row = range_match[1,3]
            # Use _n R equivalent (dplyr::row_number() or .i in collapse)
            row_number_r_expr = if(context_for_range$is_by_group) ".i" else "dplyr::row_number()"

            if (is.na(end_row)) {
                 r_in_range_cond_in_args = paste0(row_number_r_expr, " == ", start_row)
            } else {
                 r_in_range_cond_in_args = paste0(row_number_r_expr, " >= ", start_row, " & ", row_number_r_expr, " <= ", as.integer(end_row))
            }
        } else {
            return(paste0("# egen in range '", stata_in_range_in_args, "' not fully translated (f/l specifiers)."))
        }
  }


  # Combine if and in conditions within args if both exist
  final_r_subset_cond_in_args = NA_character_
  if (!is.na(r_if_cond_in_args) && !is.na(r_in_range_cond_in_args)) {
      final_r_subset_cond_in_args = paste0("(", r_if_cond_in_args, ") & (", r_in_range_cond_in_args, ")")
  } else if (!is.na(r_if_cond_in_args)) {
      final_r_subset_cond_in_args = r_if_cond_in_args
  } else if (!is.na(r_in_range_cond_in_args)) {
      final_r_subset_cond_in_args = r_in_range_cond_in_args
  }


  # Translate arguments (usually variable names)
  # Context for _n/_N etc. in arguments is the group context if by_prefix is used.
  r_egen_args = translate_stata_expression_with_r_values(egen_args_str, line_num, cmd_df, context)
   if (is.na(r_egen_args) || r_egen_args == "") {
        # This might be ok if the function takes no arguments e.g. egen group_id = group()
        if (egen_func_name != "group") { # group() takes implicit args from by() or option
             warning(paste0("Failed to translate egen arguments: ", egen_args_str))
        }
   }


  # Apply if/in condition within the function call if needed
  # Example: mean(x if y>0) -> mean(ifelse(y>0, x, NA), na.rm = TRUE)
  # This requires modifying r_egen_args based on final_r_subset_cond_in_args
  if (!is.na(final_r_subset_cond_in_args) && final_r_subset_cond_in_args != "") {
      r_egen_args_conditional = paste0("base::ifelse(", final_r_subset_cond_in_args, ", ", r_egen_args, ", NA)")
  } else {
      r_egen_args_conditional = r_egen_args
  }


  # Determine by_vars: either from `cmd_obj$by_vars` (if `bysort group: egen...`) or from `options_str` (if `egen ..., by(group)`)
  by_vars_egen = NA_character_
  if (cmd_obj$is_by_prefix && !is.na(cmd_obj$by_vars)) {
    by_vars_egen = cmd_obj$by_vars
  } else if (!is.na(options_str)) {
    by_opt_match = stringi::stri_match_first_regex(options_str, "\\bby\\s*\\(([^)]+)\\)")
    if (!is.na(by_opt_match[1,1])) {
      by_vars_egen = stringi::stri_trim_both(by_opt_match[1,2])
    }
  }

  by_vars_r_vec_str = NULL # For collapse group_by: c("var1", "var2")
  if (!is.na(by_vars_egen)) {
    by_vars_list = stringi::stri_split_regex(by_vars_egen, "\\s+")[[1]]
    by_vars_list = by_vars_list[by_vars_list != ""]
    by_vars_r_vec_str = paste0('c("', paste0(by_vars_list, collapse='", "'), '")')
  }

  # Translate egen function
  # Resulting expression for mutate: `new_var = R_EQUIVALENT_EXPRESSION`
  mutate_value_expr = ""
  is_row_function = FALSE # Flag for functions like rowtotal, rowmean that don't use group_by


  # Switch for egen functions
  if (egen_func_name == "mean") {
    mutate_value_expr = paste0("mean(", r_egen_args_conditional, ", na.rm = TRUE)")
  } else if (egen_func_name == "total" || egen_func_name == "sum") {
    mutate_value_expr = paste0("sum(", r_egen_args_conditional, ", na.rm = TRUE)")
  } else if (egen_func_name == "count") {
    # count(exp) counts non-missing results of exp. If exp is varname, sum(!is.na(varname)).
    # If exp is complex, sum(eval(parse(text=r_egen_args_conditional)) != 0 & !is.na(eval(parse(text=r_egen_args_conditional))))
    # Assuming r_egen_args_conditional results in a numeric or logical vector
    mutate_value_expr = paste0("sum(!is.na(", r_egen_args_conditional, "))")
  } else if (egen_func_name == "rank") {
    mutate_value_expr = paste0("dplyr::min_rank(", r_egen_args_conditional, ")")
  } else if (egen_func_name == "median" || egen_func_name == "p50") {
    mutate_value_expr = paste0("median(", r_egen_args_conditional, ", na.rm = TRUE)")
  } else if (egen_func_name == "sd" || egen_func_name == "std") {
    mutate_value_expr = paste0("sd(", r_egen_args_conditional, ", na.rm = TRUE)")
  } else if (egen_func_name == "group") {
    # egen id = group(var1 var2) -> var1 and var2 are in egen_args_str, space separated.
    # If by_vars_egen is present, Stata `by group: egen id = group(a b)` implies grouping by group, then within groups by a and b for the ID.
    # Stata: group(varlist) assigns unique group codes based on varlist values *within* the by group.
    # Equivalent to sorting by by_vars then varlist, then using a cumulative counter or rank.
    # If by_vars_egen is present, the actual grouping for the ID is by_vars_egen + egen_args_str.
    # If by_vars_egen is NOT present, grouping is only by egen_args_str.
    # The sorting requirement for `tag` also applies to `group`.
    # Let's use collapse::fgroup_rank which works on a vector of columns within a group.
    # The columns to group by for fgroup_rank are the arguments in egen_args_str.
    # We need to pass these as quoted variable names or expressions.
    group_vars_for_func_list = stringi::stri_split_regex(r_egen_args, "\\s+")[[1]] # Use r_egen_args (translated)
    group_vars_for_func_list = group_vars_for_func_list[group_vars_for_func_list != ""]
    group_vars_r_vec_str_for_func = paste0('c("', paste(group_vars_for_func_list, collapse='", "'), '")') # Needs to be quoted vector for select/ftag/fgroup_rank

    # The `fgroup_rank` function should be applied within the grouping defined by `by_vars_egen`.
    # mutate_value_expr = paste0("collapse::fgroup_rank(dplyr::select(., dplyr::all_of(", group_vars_r_vec_str_for_func, ")))")
    # Using `dplyr::row_number()` within a grouped mutate is simpler and common.
    # Stata `egen id = group(v)` is like `bysort v: gen id = _n`. But it also handles value labels and sorts levels.
    # A simple integer sequence per group is `1:.N` or `dplyr::row_number()`.
    # `fgroup_rank` provides the overall group rank across observations, not row number within group.
    # `dplyr::group_indices` is also for overall group rank.
    # `Stata egen group` is closer to a unique identifier for each combination of `varlist` values *within* the `by` group.
    # `by A: egen id = group(B)` means id is unique for each combination of A and B.
    # R: `data %>% group_by(A) %>% mutate(id = group_indices(.)) %>% ungroup()` or `dplyr::group_indices(., A, B)`.
    # The `group()` arguments in Stata should be added to the `by_vars_egen` for R grouping purposes.
    # Let's combine by_vars_egen and egen_args_str for grouping in R.
    all_group_vars_list = c(stringi::stri_split_regex(by_vars_egen, "\\s+")[[1]], group_vars_for_func_list)
    all_group_vars_list = all_group_vars_list[!is.na(all_group_vars_list) & all_group_vars_list != ""]
    all_group_vars_r_vec_str = paste0('c("', paste(all_group_vars_list, collapse='", "'), '")')

    # Use collapse::fgroup_rank on the combined grouping variables
    mutate_value_expr = paste0("collapse::fgroup_rank(dplyr::select(., dplyr::all_of(", all_group_vars_r_vec_str, ")))")
    by_vars_r_vec_str = NULL # The grouping for this function is handled by the argument itself, not the by_vars_egen prefix in the typical summarize sense.

  } else if (egen_func_name == "tag") {
      # egen t = tag(v1 v2) implies sorting by v1 v2 first, then tagging first obs in each group defined by v1 v2.
      # If `by group: egen t = tag(v1 v2)`, it's within `group`, then by `v1 v2`.
      # This is complex and depends on whether the bysort prefix or the tag arguments define the "group".
      # Stata manual: `tag(varlist)` marks the first observation in each group defined by `varlist` *within* the current `by` group (if any).
      # So if by_vars_egen is present, the tag is within that group. If not, it's based on the tag arguments.
      # collapse::ftag(v) flags the first observation of each group based on values of v.
      # For `tag(v1 v2)`, it's `collapse::ftag(v1, v2)`.
      tag_vars_list = stringi::stri_split_regex(r_egen_args_conditional, "\\s+")[[1]] # Use conditional args? No, tag is based on original values. Use r_egen_args.
      tag_vars_list = stringi::stri_split_regex(r_egen_args, "\\s+")[[1]]
      tag_vars_list = tag_vars_list[tag_vars_list != ""]
      tag_vars_r_vec_str = paste0('c("', paste(tag_vars_list, collapse='", "'), '")')

      if (!is.null(by_vars_r_vec_str)) {
           # If there's a by group, tag is within that group.
           # Stata `by A: egen t = tag(B C)` is equivalent to `bysort A B C: gen byte t = _n==1`.
           # We need to ensure the correct sort order *before* calculating the first observation flag.
           # The code should sort by by_vars_egen THEN tag_vars_list before calculating the flag.
           # Sorting logic should ideally be a separate step if not guaranteed by `bysort` prefix.
           # If `bysort` prefix is used, data is already sorted. If `by()` option is used, Stata implies sort.
           # Let's assume the sorting is handled and use the row number within the group.
           # `.i == 1` in collapse within fmutate and fgroup_by gives the first observation *within* the group defined by fgroup_by.
           mutate_value_expr = paste0("as.integer(.i == 1)") # Using collapse group index, converted to integer (Stata byte 0/1)

      } else {
           # No by group. Tag is based on the tag_vars only.
           # Equivalent to `bysort tag_vars: gen byte t = _n==1`.
           # Use collapse::ftag which works on selecting columns.
           mutate_value_expr = paste0("as.integer(collapse::ftag(dplyr::select(., dplyr::all_of(", tag_vars_r_vec_str, "))))")
           by_vars_r_vec_str = NULL # Tag without BY is not a grouped operation in the same sense for the R code structure.
      }


  } else if (egen_func_name == "rowtotal") {
    vars_for_rowop_list = stringi::stri_split_regex(r_egen_args, "\\s+")[[1]] # Use non-conditional args here
    vars_for_rowop_list = vars_for_rowop_list[vars_for_rowop_list != ""]
    vars_for_rowop_r_vec_str = paste0('c("', paste(vars_for_rowop_list, collapse='", "'), '")')

    # Stata rowtotal treats NA as 0 *before* summing.
    # Using rowSums on a selection of columns after replacing NA with 0.
    # `rowSums(tidyr::replace_na(dplyr::select(., dplyr::all_of(", vars_for_rowop_r_vec_str, ")), 0))`
    mutate_value_expr = paste0("rowSums(tidyr::replace_na(dplyr::select(., dplyr::all_of(", vars_for_rowop_r_vec_str, ")), 0), na.rm = FALSE)") # na.rm=FALSE because we replaced NA with 0
    is_row_function = TRUE; by_vars_r_vec_str = NULL # Row functions don't use grouping in the same way
  } else if (egen_func_name == "rowmean") {
    vars_for_rowop_list = stringi::stri_split_regex(r_egen_args, "\\s+")[[1]] # Use non-conditional args here
    vars_for_rowop_list = vars_for_rowop_list[vars_for_rowop_list != ""]
    vars_for_rowop_r_vec_str = paste0('c("', paste(vars_for_rowop_list, collapse='", "'), '")')

    mutate_value_expr = paste0("rowMeans(dplyr::select(., dplyr::all_of(", vars_for_rowop_r_vec_str, ")), na.rm = TRUE)")
    is_row_function = TRUE; by_vars_r_vec_str = NULL
  } else {
    return(paste0("# Egen function '", egen_func_name, "' not yet implemented."))
  }

  # Combine into a mutate statement
  full_mutate_expr = paste0(new_var, " = ", mutate_value_expr)

  # Build the R command string using collapse functions
  if (!is.null(by_vars_r_vec_str) && !is_row_function) {
    r_code_str = paste0("data = collapse::fgroup_by(data, ", by_vars_r_vec_str, ") %>%\n",
                        "  collapse::fmutate(", full_mutate_expr, ") %>%\n",
                        "  collapse::fungroup()")
  } else {
    r_code_str = paste0("data = collapse::fmutate(data, ", full_mutate_expr, ")")
  }

   # Add comment about options if any were present but not handled (excluding by)
   options_str_cleaned = options_str
   if (!is.na(options_str_cleaned)) {
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "\\bby\\s*\\([^)]+\\)", "")
        options_str_cleaned = stringi::stri_trim_both(stringi::stri_replace_all_regex(options_str_cleaned, ",+", ",")) # Clean up multiple commas
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "^,+", "") # Remove leading comma
   }

   if (!is.na(options_str_cleaned) && options_str_cleaned != "") {
        r_code_str = paste0(r_code_str, paste0(" # Other options ignored: ", options_str_cleaned))
   }


  return(r_code_str)
}


