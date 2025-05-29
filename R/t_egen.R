t_egen = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_egen")
  # Basic parsing: newvar = function(args) [, by(groupvars)] [if condition]
  # Example: egen mean_i_grp = mean(i), by(group)
  # Example: egen total_i = total(i)
  # Example: bysort group: egen rank_i = rank(i) (Note: bysort handled by cmd_obj$is_by_prefix)

  # Remove type prefix if any (byte, int, long, float, double, str#, etc.)
  # Pattern: ^\s*(byte|int|long|float|double|str\\d+)\\s+
  rest_of_cmd_no_type = stringi::stri_replace_first_regex(rest_of_cmd, "^\\s*(?:byte|int|long|float|double|str\\d+|strL)\\s+", "")

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

  # Look for `if` first
  if_match_in_args = stringi::stri_match_first_regex(egen_args_str, "\\s+if\\s+(.*)$")
   if(!is.na(if_match_in_args[1,1])) {
      stata_if_cond_in_args = if_match_in_args[1,2]
      egen_args_str = stringi::stri_replace_last_regex(egen_args_str, "\\s+if\\s+(.*)$", "")
      egen_args_str = stringi::stri_trim_both(egen_args_str)
   }

  # Check for `in`
  in_match_in_args = stringi::stri_match_first_regex(egen_args_str, "\\s+in\\s+(.*)$")
   if(!is.na(in_match_in_args[1,1])) {
      stata_in_range_in_args = in_match_in_args[1,2] # Corrected from if_match_in_args[1,2]
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
            row_number_r_expr = "as.numeric(dplyr::row_number())" # This will be translated based on context

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
      # Stata's `if` condition treats NA as FALSE.
      r_egen_args_conditional = paste0("dplyr::if_else(dplyr::coalesce(", final_r_subset_cond_in_args, ", FALSE), ", r_egen_args, ", NA)")
  } else {
      r_egen_args_conditional = r_egen_args
  }

  # Determine if 'fieldstrustmissings' option is present
  # FIX: Use dplyr::coalesce for robustness against NA in options_str
  is_fieldstrustmissings = dplyr::coalesce(stringi::stri_detect_fixed(options_str, "fieldstrustmissings"), FALSE)


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
    # Stata rank() without fieldstrustmissings returns missing for missing.
    # Stata rank() with fieldstrustmissings treats missing values as true values (usually largest) and assigns them a rank.
    # Stata's rank() uses the 'average' method for ties.
    if (is_fieldstrustmissings) {
      # Replace NA values with Inf to rank them highest.
      # Note: r_egen_args_conditional already applies if/in conditions, yielding NA for rows not meeting condition.
      # These NAs should also be treated as largest for ranking due to fieldstrustmissings.
      val_for_ranking = paste0("as.numeric(dplyr::if_else(is.na(", r_egen_args_conditional, "), Inf, ", r_egen_args_conditional, "))")
      calc_expr = paste0("as.numeric(base::rank(", val_for_ranking, ", ties.method = 'average', na.last = 'keep'))")
    } else {
      # Default Stata rank: NAs get NA ranks, and uses 'average' method for ties.
      calc_expr = paste0("as.numeric(base::rank(", r_egen_args_conditional, ", ties.method = 'average', na.last = 'keep'))")
    }
  } else if (egen_func_name == "median" || egen_func_name == "p50") {
    calc_expr = paste0("stats::median(", r_egen_args_conditional, ", na.rm = TRUE)")
  } else if (egen_func_name == "sd" || egen_func_name == "std") {
    calc_expr = paste0("stats::sd(", r_egen_args_conditional, ", na.rm = TRUE)")
  } else if (egen_func_name == "group") {
    # dplyr::cur_group_id() gives integer for each group.
    calc_expr = paste0("dplyr::cur_group_id()")
  } else if (egen_func_name == "tag") {
    # Stata tag(varlist) creates 1 for first observation in a group (defined by varlist) and 0 otherwise.
    calc_expr = paste0("as.numeric(dplyr::row_number() == 1)")
  } else if (egen_func_name == "rowtotal") {
    # FIX: Remove backticks from variable names as `translate_stata_expression_to_r` already adds them,
    # and `dplyr::all_of` expects bare column names (strings), not backticked strings.
    vars_for_rowop_list = stringi::stri_split_regex(r_egen_args, "\\s+")[[1]] # Use non-conditional args here
    vars_for_rowop_list = vars_for_rowop_list[!is.na(vars_for_rowop_list) & vars_for_rowop_list != ""] # Filter empty/NA
    vars_for_rowop_list = stringi::stri_replace_all_fixed(vars_for_rowop_list, "`", "") # Remove backticks

    # Stata rowtotal treats NA as 0 *before* summing.
    # Replace NA with 0 in the selected columns before summing.
    cols_selection_expr = paste0("dplyr::select(dplyr::cur_data_all(), dplyr::all_of(c('", paste(vars_for_rowop_list, collapse="','"), "')))")
    calc_expr = paste0("base::rowSums(", cols_selection_expr, " %>% replace(is.na(.), 0), na.rm = FALSE)")
    is_row_function = TRUE
  } else if (egen_func_name == "rowmean") {
    # FIX: Remove backticks from variable names for `dplyr::all_of`.
    vars_for_rowop_list = stringi::stri_split_regex(r_egen_args, "\\s+")[[1]] # Use non-conditional args here
    vars_for_rowop_list = vars_for_rowop_list[!is.na(vars_for_rowop_list) & vars_for_rowop_list != ""] # Filter empty/NA
    vars_for_rowop_list = stringi::stri_replace_all_fixed(vars_for_rowop_list, "`", "") # Remove backticks

    # Stata rowmean ignores NAs. base::rowMeans with na.rm = TRUE achieves this.
    cols_selection_expr = paste0("dplyr::select(dplyr::cur_data_all(), dplyr::all_of(c('", paste(vars_for_rowop_list, collapse="','"), "')))")
    calc_expr = paste0("base::rowMeans(", cols_selection_expr, ", na.rm = TRUE)")
    is_row_function = TRUE
  } else if (egen_func_name == "concat") {
    vars_to_concat_list = stringi::stri_split_regex(egen_args_str, "\\s+")[[1]]
    vars_to_concat_list = vars_to_concat_list[!is.na(vars_to_concat_list) & vars_to_concat_list != ""]

    if (length(vars_to_concat_list) == 0) {
      return(paste0("# egen concat() requires variables to concatenate."))
    }

    # Stata: If all variables in varlist are missing, newvar is missing. Otherwise, missing values are treated as empty strings.
    # Approach:
    # 1. Check if all input variables for a row are NA. If so, result is NA.
    # 2. Otherwise, for each variable, replace NA with "" and then concatenate.

    # Expression to check if all relevant variables in a row are NA.
    # Use data[['var_name']] for explicit column access.
    all_vars_na_check_list = paste0("is.na(data[['", vars_to_concat_list, "']])")
    all_vars_na_check_expr = paste0("(", paste0(all_vars_na_check_list, collapse = " & "), ")")

    # Arguments for stri_paste, with NAs replaced by empty strings
    stri_paste_args_with_na_empty = paste0("dplyr::if_else(is.na(as.character(data[['", vars_to_concat_list, "']])), \"\", as.character(data[['", vars_to_concat_list, "']]))", collapse = ", ")

    # Expression for the actual concatenation
    # Use na_empty = FALSE (default) because NAs are already handled.
    calc_expr = paste0("dplyr::if_else(", all_vars_na_check_expr, ", NA_character_, stringi::stri_paste(", stri_paste_args_with_na_empty, ", sep = ''))")

    is_row_function = TRUE # Concatenation is inherently row-wise.
  } else {
    return(paste0("# Egen function '", egen_func_name, "' not yet implemented."))
  }

  # Combine into a mutate statement
  full_mutate_expr = paste0("`", new_var, "` = ", calc_expr)


  # Determine actual grouping variables for dplyr::group_by
  group_vars_list_bare = character(0) # Will hold just bare variable names
  if (cmd_obj$is_by_prefix) {
    if (length(cmd_obj$by_group_vars) > 0 && !is.na(cmd_obj$by_group_vars[1])) {
      group_vars_list = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
      group_vars_list = group_vars_list[!is.na(group_vars_list) & group_vars_list != ""]
      if (length(group_vars_list) > 0) {
        group_vars_list_bare = group_vars_list # Assign just the bare names
      }
    }
  } else if (!is.na(options_str)) {
    by_opt_match = stringi::stri_match_first_regex(options_str, "\\bby\\s*\\(([^)]+)\\)")
    if (!is.na(by_opt_match[1,1])) {
      group_vars_list_bare = stringi::stri_split_regex(stringi::stri_trim_both(by_opt_match[1,2]), "\\s+")[[1]]
      group_vars_list_bare = group_vars_list_bare[!is.na(group_vars_list_bare) & group_vars_list_bare != ""]
    }
  }

  # For 'group' and 'tag' functions, the arguments also define the grouping
  if (egen_func_name %in% c("group", "tag")) {
    egen_func_args_list = stringi::stri_split_regex(egen_args_str, "\\s+")[[1]]
    egen_func_args_list = egen_func_args_list[!is.na(egen_func_args_list) & egen_func_args_list != ""]
    # Union of `by` variables and `egen` function arguments defines the group
    group_vars_list_bare = unique(c(group_vars_list_bare, egen_func_args_list))
  }


  # Determine variables for initial sorting for functions requiring internal sort (rank, group, tag)
  sort_vars_for_arrange = character(0)

  # Only sort temporarily if it's NOT a by-prefix command, but one of the order-sensitive egen functions
  if (!cmd_obj$is_by_prefix && length(group_vars_list_bare) > 0 && egen_func_name %in% c("rank", "group", "tag")) {
    sort_vars_for_arrange = unique(c(group_vars_list_bare))
    if (egen_func_name == "rank" && !is.na(egen_args_str) && egen_args_str != "") {
      # For rank, also sort by the argument of rank() if it's a variable
      rank_arg_var = stringi::stri_replace_all_fixed(r_egen_args, "`", "") # Remove backticks for comparison
      if (rank_arg_var %in% names(data)) { # Check if it's a valid variable name
        sort_vars_for_arrange = unique(c(sort_vars_for_arrange, rank_arg_var))
      }
    }
    sort_vars_for_arrange = sort_vars_for_arrange[!is.na(sort_vars_for_arrange) & sort_vars_for_arrange != ""]
  }

  r_code_lines = c()
  pipe_elements = list("data") # Start the pipe with the data object

  # Add arrange for `egen group/tag/rank` functions within the pipe, if not already handled by bysort command
  if (length(sort_vars_for_arrange) > 0 && !is_row_function) {
    arrange_vars_expr = paste0('!!!dplyr::syms(c("', paste0(sort_vars_for_arrange, collapse = '", "'), '"))')
    pipe_elements = c(pipe_elements, paste0("dplyr::arrange(", arrange_vars_expr, ")"))
  }


  # Add grouping and mutate steps
  if (length(group_vars_list_bare) > 0 && !is_row_function) {
    group_by_expr = paste0('dplyr::group_by(!!!dplyr::syms(c("', paste0(group_vars_list_bare, collapse='", "'), '")))')
    pipe_elements = c(pipe_elements, group_by_expr)
  }

  pipe_elements = c(pipe_elements, paste0("dplyr::mutate(", full_mutate_expr, ")"))

  if (length(group_vars_list_bare) > 0 && !is_row_function) {
    pipe_elements = c(pipe_elements, "dplyr::ungroup()")
  }

  # Restore original order if it was a temporary sort for egen functions without bysort prefix
  # This is handled by arranging by `stata2r_original_order_idx` after ungrouping.
  if (!cmd_obj$is_by_prefix && length(sort_vars_for_arrange) > 0 && !is_row_function) {
    pipe_elements = c(pipe_elements, "dplyr::arrange(stata2r_original_order_idx)")
  }

  r_code_lines = c(r_code_lines, paste0("data = ", paste(pipe_elements, collapse = " %>% \n  ")))

   # Add comment about options if any were present but not handled (excluding by)
   options_str_cleaned = options_str
   if (!is.na(options_str_cleaned)) {
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "\\bby\\s*\\([^)]+\\)", "")
        # Remove fieldstrustmissings from the options string if it was present
        options_str_cleaned = stringi::stri_replace_first_fixed(options_str_cleaned, "fieldstrustmissings", "")
        options_str_cleaned = stringi::stri_trim_both(stringi::stri_replace_all_regex(options_str_cleaned, ",+", ",")) # Clean up multiple commas
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "^,+", "") # Remove leading comma
   }


   if (!is.na(options_str_cleaned) && options_str_cleaned != "") {
        r_code_lines = c(r_code_lines, paste0(" # Other options ignored: ", options_str_cleaned))
   }


  return(paste(r_code_lines, collapse="\n"))
}


