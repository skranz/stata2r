t_egen = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_egen")
  # Basic parsing: newvar = function(args) [, by(groupvars)] [if condition]
  # Example: egen mean_i_grp = mean(i), by(group)
  # Example: egen total_i = total(i)
  # Example: bysort group: egen rank_i = rank(i) (Note: bysort handled by cmd_obj$is_by_prefix)

  # Remove type prefix if any (byte, int, long, float, double, str#, etc.)
  # Pattern: ^\s*(byte|int|long|float|double|str\\d+)\\s+
  rest_of_cmd_no_type = stringi::stri_replace_first_regex(rest_of_cmd, "^\\s*(?:byte|int|long|float|double|str\\d+)\\s+", "")

  # Re-parse rest_of_cmd_no_type looking for `newvar = fcn(args) [if cond] [, options]`
  # Split at the first `=`. Left is `newvar`. Right is `fcn(args) [if cond] [, options]`
  parts_eq = stringi::stri_split_fixed(rest_of_cmd_no_type, "=", n=2)[[1]]
  if(length(parts_eq) != 2) return(paste0("# Failed to parse egen command structure (no =): ", rest_of_cmd))

  new_var = stringi::stri_trim_both(parts_eq[1])
  right_part = stringi::stri_trim_both(parts_eq[2])

  # Split right_part at the first comma (if any) to separate function/args/if from options
  parts_comma_list = stringi::stri_split_fixed(right_part, ",", n=2)
  parts_comma = parts_comma_list[[1]]

  if(length(parts_comma) != 2) {
    func_args_if_part = stringi::stri_trim_both(parts_comma[1])
    options_str = NA_character_
  } else {
    func_args_if_part = stringi::stri_trim_both(parts_comma[1])
    options_str = stringi::stri_trim_both(parts_comma[2])
  }


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
       range_match = stringi::stri_match_first_regex(stata_in_range_in_args, "^(\\d+)(?:/(\\d+))?$")
        if (!is.na(range_match[1,1])) {
            start_row = as.integer(range_match[1,2])
            end_row = range_match[1,3]
            # Use dplyr::row_number(), as stata_expression_translator will handle _n
            row_number_r_expr = "dplyr::row_number()" # This will be translated based on context

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


  # Determine the base grouping variables for dplyr::group_by (from by-prefix or by() option)
  by_vars_for_group_by = NULL
  by_vars_list_unquoted = character(0) # Initialize for use in combined_grouping_vars

  if (cmd_obj$is_by_prefix) {
    if (!is.na(cmd_obj$by_group_vars)) {
      by_vars_list_unquoted = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
      by_vars_list_unquoted = by_vars_list_unquoted[by_vars_list_unquoted != ""]
      by_vars_for_group_by = paste0('c("', paste0(by_vars_list_unquoted, collapse='", "'), '")')
    }

    sort_vars_list = character(0)
    if (!is.na(cmd_obj$by_sort_vars)) {
      sort_vars_list = stringi::stri_split_fixed(cmd_obj$by_sort_vars, ",")[[1]]
      sort_vars_list = sort_vars_list[sort_vars_list != ""]
    }

    # If there are sort keys for by-processing, prepare the arrange call
    # This is handled by t_generate/t_replace for _n/_N usage, but egen functions might need sorting too.
    # Stata egen functions like `rank` are influenced by sort order if `by` prefix is used.
    # However, for functions like mean/total, explicit sorting isn't strictly necessary for the result,
    # but `by` prefix implies it.
    # The `context$is_by_group` comes from the `by` prefix.
    # The `dplyr::group_by` handles the grouping.
    # For `rank`, `_n`, `_N` etc. the order within groups matters.
    # The `stata_expression_translator` should handle `_n` and `_N` correctly by replacing with `fseq()` and `fnobs()` inside grouped operations.
    # So explicit `arrange` here might be redundant or problematic if the order is already handled by `by` prefix parsing.
    # Let's assume that `dplyr::group_by` and `stata_expression_translator` are sufficient.
  } else if (!is.na(options_str)) {
    by_opt_match = stringi::stri_match_first_regex(options_str, "\\bby\\s*\\(([^)]+)\\)")
    if (!is.na(by_opt_match[1,1])) {
      by_vars_list_unquoted = stringi::stri_split_regex(stringi::stri_trim_both(by_opt_match[1,2]), "\\s+")[[1]]
      by_vars_list_unquoted = by_vars_list_unquoted[by_vars_list_unquoted != ""]
      by_vars_for_group_by = paste0('c("', paste0(by_vars_list_unquoted, collapse='", "'), '")')
    }
  }

  # Translate egen function into an R expression for calculation
  calc_expr = ""
  is_row_function = FALSE # Flag for functions like rowtotal, rowmean that don't use group_by


  # Switch for egen functions
  if (egen_func_name == "mean") {
    calc_expr = paste0("mean(", r_egen_args_conditional, ", na.rm = TRUE)")
  } else if (egen_func_name == "total" || egen_func_name == "sum") {
    calc_expr = paste0("sum(", r_egen_args_conditional, ", na.rm = TRUE)")
  } else if (egen_func_name == "count") {
    # count(exp) counts non-missing results of exp. If exp is varname, sum(!is.na(varname)).
    # If exp is complex, sum(eval(parse(text=r_egen_args_conditional)) != 0 & !is.na(eval(parse(text=r_egen_args_conditional))))
    # Assuming r_egen_args_conditional results in a numeric or logical vector
    calc_expr = paste0("sum(!is.na(", r_egen_args_conditional, "))")
  } else if (egen_func_name == "rank") {
    calc_expr = paste0("dplyr::min_rank(", r_egen_args_conditional, ")")
  } else if (egen_func_name == "median" || egen_func_name == "p50") {
    calc_expr = paste0("median(", r_egen_args_conditional, ", na.rm = TRUE)")
  } else if (egen_func_name == "sd" || egen_func_name == "std") {
    calc_expr = paste0("sd(", r_egen_args_conditional, ", na.rm = TRUE)")
  } else if (egen_func_name == "group" || egen_func_name == "tag") {
    # For 'group' and 'tag', the effective grouping for dplyr::group_by is the combination
    # of the 'by' prefix/option variables and the variables in the egen function arguments.
    egen_func_args_list = stringi::stri_split_regex(egen_args_str, "\\s+")[[1]]
    egen_func_args_list = egen_func_args_list[egen_func_args_list != ""]

    combined_grouping_vars = unique(c(by_vars_list_unquoted, egen_func_args_list))
    combined_grouping_vars = combined_grouping_vars[combined_grouping_vars != ""]

    if (length(combined_grouping_vars) > 0) {
      by_vars_for_group_by = paste0('c("', paste0(combined_grouping_vars, collapse='", "'), '")')
    } else {
      by_vars_for_group_by = NULL # No grouping if no vars for group/tag
    }

    if (egen_func_name == "group") {
        # dplyr::cur_group_id() gives integer for each group.
        calc_expr = paste0("dplyr::cur_group_id()")
    } else if (egen_func_name == "tag") {
        # Stata `tag` flags the first obs in each group defined by `varlist` (and `by` prefix if any).
        # This is `_n==1` after sorting by all these variables.
        # _n is translated by stata_expression_translator_to_r, which will use collapse::fseq() if grouped.
        calc_expr = paste0("as.integer(dplyr::row_number() == 1)") # This will be translated to fseq() if grouped.
    }
  } else if (egen_func_name == "rowtotal") {
    vars_for_rowop_list = stringi::stri_split_regex(r_egen_args, "\\s+")[[1]] # Use non-conditional args here
    vars_for_rowop_list = vars_for_rowop_list[vars_for_rowop_list != ""]
    vars_for_rowop_r_vec_str = paste0('c("', paste(vars_for_rowop_list, collapse='", "'), '")')

    # Stata rowtotal treats NA as 0 *before* summing.
    # Using rowSums on a selection of columns after replacing NA with 0.
    calc_expr = paste0("rowSums(tidyr::replace_na(dplyr::select(dplyr::cur_data_all(), dplyr::all_of(", vars_for_rowop_r_vec_str, ")), 0), na.rm = FALSE)") # na.rm=FALSE because we replaced NA with 0
    is_row_function = TRUE; by_vars_for_group_by = NULL # Row functions don't use grouping in the same way
  } else if (egen_func_name == "rowmean") {
    vars_for_rowop_list = stringi::stri_split_regex(r_egen_args, "\\s+")[[1]] # Use non-conditional args here
    vars_for_rowop_list = vars_for_rowop_list[vars_for_rowop_list != ""]
    vars_for_rowop_r_vec_str = paste0('c("', paste(vars_for_rowop_list, collapse='", "'), '")')

    calc_expr = paste0("rowMeans(dplyr::select(dplyr::cur_data_all(), dplyr::all_of(", vars_for_rowop_r_vec_str, ")), na.rm = TRUE)")
    is_row_function = TRUE; by_vars_for_group_by = NULL
  } else {
    return(paste0("# Egen function '", egen_func_name, "' not yet implemented."))
  }

  # Combine into a mutate statement
  full_mutate_expr = paste0(new_var, " = ", calc_expr)

  # Build the R command string using pipes
  r_code_lines = c("data = data %>%") # Start with data and the first pipe FIX: removed \n

  if (!is.null(by_vars_for_group_by) && length(by_vars_list_unquoted) > 0 && !is_row_function) {
    r_code_lines = c(r_code_lines,
                        "  dplyr::group_by(dplyr::across(", by_vars_for_group_by, ")) %>%", # FIX: removed \n
                        "  dplyr::mutate(", full_mutate_expr, ") %>%", # FIX: removed \n
                        "  dplyr::ungroup()")
  } else {
    r_code_lines = c(r_code_lines,
                        "  dplyr::mutate(", full_mutate_expr, ")")
  }


   # Add comment about options if any were present but not handled (excluding by)
   options_str_cleaned = options_str
   if (!is.na(options_str_cleaned)) {
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "\\bby\\s*\\([^)]+\\)", "")
        options_str_cleaned = stringi::stri_trim_both(stringi::stri_replace_all_regex(options_str_cleaned, ",+", ",")) # Clean up multiple commas
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "^,+", "") # Remove leading comma
   }

   if (!is.na(options_str_cleaned) && options_str_cleaned != "") {
        r_code_lines = c(r_code_lines, paste0(" # Other options ignored: ", options_str_cleaned))
   }


  return(paste(r_code_lines, collapse="\n"))
}

