# Translate Stata 'expand' command
# Stata: expand N [if] [in] [, options]
# Creates N copies of each observation. N can be an expression.
# Example: expand 2
# Example: expand num_copies if condition

t_expand = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_expand") # Added restore.point
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  parts_if = stringi::stri_split_regex(rest_of_cmd_trimmed, "\\s+if\\s+", n=2)[[1]]
  expand_n_part_or_cond = stringi::stri_trim_both(parts_if[1])
  stata_if_cond = if(length(parts_if) > 1) stringi::stri_trim_both(parts_if[2]) else NA_character_

  parts_in = stringi::stri_split_regex(expand_n_part_or_cond, "\\s+in\\s+", n=2)[[1]]
  stata_n_expr = stringi::stri_trim_both(parts_in[1])
  stata_in_range = if(length(parts_in) > 1) stringi::stri_trim_both(parts_in[2]) else NA_character_

  if (is.na(stata_n_expr) || stata_n_expr == "") {
       return(paste0("# expand command requires N expression: ", rest_of_cmd))
  }

  r_n_expr = translate_stata_expression_with_r_values(stata_n_expr, line_num, cmd_df, context = list(is_by_group=FALSE))
   if (is.na(r_n_expr) || r_n_expr == "") {
       return(paste0("# Failed to translate N expression for expand: ", stata_n_expr))
   }

  r_if_cond = NA_character_
  if (!is.na(stata_if_cond) && stata_if_cond != "") {
    r_if_cond = translate_stata_expression_with_r_values(stata_if_cond, line_num, cmd_df, context = list(is_by_group = FALSE))
  }

  r_in_range_cond = NA_character_
  if (!is.na(stata_in_range) && stata_in_range != "") {
       range_match = stringi::stri_match_first_regex(stata_in_range, "^(\\d+)(?:/(\\d+))?$")
        if (!is.na(range_match[1,1])) {
            start_row = as.integer(range_match[1,2])
            end_row = range_match[1,3]
            # Changed to dplyr::row_number() to align with _n translation
            if (is.na(end_row)) {
                 r_in_range_cond = paste0("dplyr::row_number() == ", start_row)
            } else {
                 r_in_range_cond = paste0("dplyr::row_number() >= ", start_row, " & dplyr::row_number() <= ", as.integer(end_row))
            }
        } else {
            return(paste0("# expand in range '", stata_in_range, "' not fully translated (f/l specifiers)."))
        }
  }

  final_r_subset_cond = NA_character_
  if (!is.na(r_if_cond) && !is.na(r_in_range_cond)) {
      final_r_subset_cond = paste0("(", r_if_cond, ") & (", r_in_range_cond, ")")
  } else if (!is.na(r_if_cond)) {
      final_r_subset_cond = r_if_cond
  } else if (!is.na(r_in_range_cond)) {
      final_r_subset_cond = r_in_range_cond
  }

  r_code_str = ""
  line_id = cmd_obj$line # Actual line number from the do-file for unique temp var names

  if (!is.na(final_r_subset_cond) && final_r_subset_cond != "") {
       temp_n_values_var = paste0("__expand_n_values_L", line_id)
       temp_cond_values_var = paste0("__expand_cond_values_L", line_id)
       final_times_calc_var = paste0("__final_expand_times_L", line_id)

       r_code_lines = c(
           paste0(temp_n_values_var, " = ", r_n_expr),
           paste0(temp_cond_values_var, " = ", final_r_subset_cond),
           paste0(final_times_calc_var, " = ifelse(!is.na(", temp_cond_values_var, ") & ", temp_cond_values_var, ", ",
                                             "ifelse(is.na(", temp_n_values_var, "), 1, pmax(0, as.integer(", temp_n_values_var, "))), ", # ensure N is non-negative integer, 1 if NA
                                             "1)"), # if condition not met, expand 1 time (keep original)
           paste0(final_times_calc_var, " = ifelse(is.na(", final_times_calc_var, "), 1, ", final_times_calc_var, ")"), # Ensure final times is not NA
           paste0("data = data[base::rep(1:NROW(data), times = ", final_times_calc_var, "), ]"),
           paste0("if (exists('", temp_n_values_var, "')) rm(", temp_n_values_var, ", ", temp_cond_values_var, ", ", final_times_calc_var, ")")
       )
       r_code_str = paste(r_code_lines, collapse="\n")
  } else {
       temp_n_values_var = paste0("__expand_n_values_L", line_id)
       final_times_calc_var = paste0("__final_expand_times_L", line_id)
        r_code_lines = c(
           paste0(temp_n_values_var, " = (", r_n_expr, ")"),
           paste0(final_times_calc_var, " = ifelse(is.na(", temp_n_values_var, "), 1, pmax(0, as.integer(", temp_n_values_var, ")))"), # ensure N is non-negative integer, 1 if NA
           paste0("data = data[base::rep(1:NROW(data), times = ", final_times_calc_var, "), ]"),
           paste0("if (exists('", temp_n_values_var, "')) rm(", temp_n_values_var, ", ", final_times_calc_var, ")")
        )
       r_code_str = paste(r_code_lines, collapse="\n")
  }

  options_str_cleaned = NA_character_ # expand has few options, not handled yet.
  # If options_str was parsed from original cmd:
  # options_str_cleaned = options_str

   if (!is.na(options_str_cleaned) && options_str_cleaned != "") {
        r_code_str = paste0(r_code_str, paste0(" # Options ignored: ", options_str_cleaned))
   }

  return(r_code_str)
}

