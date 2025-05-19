# Translate Stata 'expand' command
# Stata: expand N [if] [in] [, options]
# Creates N copies of each observation. N can be an expression.
# Example: expand 2
# Example: expand num_copies if condition

t_expand = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # Parse N expression and optional if/in
  # Pattern: ^\s*(.*?)(?:\\s+if\\s+(.*?))?(?:\\s+in\\s+(.*?))?$ # if/in order can vary, simpler regex
  # Let's split at the first space that is *not* inside if/in. This is complex.
  # Simpler: Assume N is the first part, then look for optional `if` or `in`.

  parts_if = stringi::stri_split_regex(rest_of_cmd_trimmed, "\\s+if\\s+", n=2)[[1]]
  expand_n_part = stringi::stri_trim_both(parts_if[1])
  stata_if_cond = if(length(parts_if) > 1) stringi::stri_trim_both(parts_if[2]) else NA_character_

  parts_in = stringi::stri_split_regex(expand_n_part, "\\s+in\\s+", n=2)[[1]]
  expand_n_part = stringi::stri_trim_both(parts_in[1])
  stata_in_range = if(length(parts_in) > 1) stringi::stri_trim_both(parts_in[2]) else NA_character_

  stata_n_expr = expand_n_part
  if (is.na(stata_n_expr) || stata_n_expr == "") {
       return(paste0("# expand command requires N expression: ", rest_of_cmd))
  }

  # Translate N expression (can be variable or number)
  # Context for _n/_N etc. here is the original data.
  r_n_expr = translate_stata_expression_with_r_values(stata_n_expr, line_num, cmd_df, context = list(is_by_group=FALSE))
   if (is.na(r_n_expr) || r_n_expr == "") {
       return(paste0("# Failed to translate N expression for expand: ", stata_n_expr))
   }


  # Translate if condition
  r_if_cond = NA_character_
  if (!is.na(stata_if_cond) && stata_if_cond != "") {
    r_if_cond = translate_stata_expression_with_r_values(stata_if_cond, line_num, cmd_df, context = list(is_by_group = FALSE))
  }

  # Translate in range (less common with expand, often done with if/in before)
  r_in_range_cond = NA_character_
  if (!is.na(stata_in_range) && stata_in_range != "") {
       # Stata `in f/l` refers to row numbers.
       range_match = stringi::stri_match_first_regex(stata_in_range, "^(\\d+)(?:/(\\d+))?$")
        if (!is.na(range_match[1,1])) {
            start_row = as.integer(range_match[1,2])
            end_row = range_match[1,3]
            if (is.na(end_row)) {
                 r_in_range_cond = paste0("dplyr::row_number() == ", start_row)
            } else {
                 r_in_range_cond = paste0("dplyr::row_number() >= ", start_row, " & dplyr::row_number() <= ", as.integer(end_row))
            }
        } else {
            return(paste0("# expand in range '", stata_in_range, "' not fully translated (f/l specifiers)."))
        }
  }

  # Combine if and in conditions if both exist
  final_r_subset_cond = NA_character_
  if (!is.na(r_if_cond) && !is.na(r_in_range_cond)) {
      final_r_subset_cond = paste0("(", r_if_cond, ") & (", r_in_range_cond, ")")
  } else if (!is.na(r_if_cond)) {
      final_r_subset_cond = r_if_cond
  } else if (!is.na(r_in_range_cond)) {
      final_r_subset_cond = r_in_range_cond
  }


  # R equivalent: use base::rep() on row indices.
  # The index vector will be 1 1 ... 2 2 ... N N ...
  # The number of repetitions for each row depends on the expression N and the if/in condition.
  # For rows where the condition is met, repeat N times. For rows where it's not met, repeat 1 time.
  # The N expression itself can be conditional (e.g., `expand num_copies if group=="A"`).
  # Example: `expand 2` -> `rep(1:NROW(data), each=2)` or `rep(1:NROW(data), times=2)` depending on desired order.
  # Stata `expand N` maintains original sort order by default (like `rep(..., each=N)`).
  # Example: `expand N if condition` -> `rep(1:NROW(data), times = ifelse(condition, N, 1))`

  # Build the `times` argument for rep()
  times_expr = r_n_expr

  if (!is.na(final_r_subset_cond) && final_r_subset_cond != "") {
      # Apply the condition: repeat N if cond, 1 otherwise
       # Need to evaluate the condition and the N expression row-wise on the original data.
       # This is hard to do directly in a single rep() call that operates on row indices.
       # Alternative: Create a temporary variable with the desired repetitions per row.
       # `data = data %>% mutate(__expand_times = ifelse(condition, N, 1))`
       # Then use `data[rep(1:NROW(data), times = data$__expand_times), ]`
       # However, `data` is the dataframe being transformed. We need the old `data` inside mutate.

       # A better approach: evaluate the times vector and the condition vector first, then use rep.
       # Let's generate R code that does this.
       temp_times_var = paste0("__expand_times_L", cmd_obj$line)
       temp_cond_var = paste0("__expand_cond_L", cmd_obj$line)

       r_code_lines = c(
           paste0(temp_times_var, " = (", r_n_expr, ")"), # Evaluate N expression
           paste0(temp_cond_var, " = (", final_r_subset_cond, ")"), # Evaluate condition
           # If condition is met, use temp_times_var. Otherwise, use 1.
           # Handle NA in condition: if condition is NA, row is not expanded.
           paste0(temp_times_final = "ifelse(!is.na(", temp_cond_var, ") & ", temp_cond_var, ", ", temp_times_var, ", 1)"),
           # Handle NA in N expression itself: Stata expand N where N is missing gives error.
           # R rep(..., times=NA) gives error. Need to handle.
           paste0(temp_times_final = "ifelse(is.na(", temp_times_final, "), 1, ", temp_times_final, ")"), # If N is NA, treat as 1? Or error? Stata errors. Let's allow 1 for NA N.
           # Repeat rows using the calculated times
           "data = data[base::rep(1:NROW(data), times = ", temp_times_final, "), ]",
           # Clean up temporary variables if they were created within the environment
           paste0("rm(", temp_times_var, ", ", temp_cond_var, ", ", temp_times_final, ")") # This assumes translation environment is function scope
           # If translation env is global, need to be careful with temp vars. Assume function scope for now.
       )
       r_code_str = paste(r_code_lines, collapse="\n")


  } else {
       # No if/in condition, just expand N times
       # Check if N is a single number or a variable/expression
       # If N is a variable/expression, Stata expands each observation N[i] times.
       # Example: `expand income_value` expands row i income_value[i] times.
       # This is the same logic as the conditional case, but without the condition.
       temp_times_var = paste0("__expand_times_L", cmd_obj$line)
        r_code_lines = c(
           paste0(temp_times_var, " = (", r_n_expr, ")"), # Evaluate N expression
           # Handle NA in N expression itself
           paste0(temp_times_var, " = ifelse(is.na(", temp_times_var, "), 1, ", temp_times_var, ")"),
           # Repeat rows using the calculated times
           "data = data[base::rep(1:NROW(data), times = ", temp_times_var, "), ]",
           # Clean up temporary variables
           paste0("rm(", temp_times_var, ")")
        )
       r_code_str = paste(r_code_lines, collapse="\n")

  }

  # Add comment about options if any were present but not handled
   options_str_cleaned = options_str # No options handled currently

   if (!is.na(options_str_cleaned) && options_str_cleaned != "") {
        r_code_str = paste0(r_code_str, paste0(" # Options ignored: ", options_str_cleaned))
   }


  return(r_code_str)
}

