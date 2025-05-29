# Translate Stata 'destring' command
# Stata: destring varlist [if] [in] , {generate(newvarlist) | replace} [options]
# Converts string variables into numeric variables.

t_destring = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_destring") # Added restore.point
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # Parse varlist, if/in, options (generate() or replace)
  # Pattern: `varlist [if] [in] , options`

  varlist_str = NA_character_
  stata_if_in_cond = NA_character_
  options_str = NA_character_

  # Find if/in first
  if_in_match = stringi::stri_match_first_regex(rest_of_cmd_trimmed, "\\s+(?:if\\s+|in\\s+)(.*)$")
  if(!is.na(if_in_match[1,1])) {
      stata_if_in_cond = if_in_match[1,2]
      rest_no_if_in = stringi::stri_replace_last_regex(rest_of_cmd_trimmed, "\\s+(?:if\\s+|in\\s+)(.*)$", "")
      rest_no_if_in = stringi::stri_trim_both(rest_no_if_in)
  } else {
      rest_no_if_in = rest_of_cmd_trimmed
  }

   # Find options (required)
  options_match = stringi::stri_match_first_regex(rest_no_if_in, ",\\s*(.*)$")
  if (!is.na(options_match[1,1])) {
      options_str = stringi::stri_trim_both(options_match[1,2])
      varlist_str = stringi::stri_replace_last_regex(rest_no_if_in, ",\\s*(.*)$", "")
      varlist_str = stringi::stri_trim_both(varlist_str)
  } else {
      return(paste0("# destring command requires options (, gen() or , replace): ", rest_of_cmd))
  }

   if (is.na(varlist_str) || varlist_str == "") {
       return(paste0("# destring command requires varlist: ", rest_of_cmd))
   }

  vars_to_destring = stringi::stri_split_regex(varlist_str, "\\s+")[[1]]
  vars_to_destring = vars_to_destring[vars_to_destring != ""]
   if (length(vars_to_destring) == 0) {
       return(paste0("# destring command requires non-empty varlist: ", rest_of_cmd))
   }


  # Parse options, specifically `generate()` or `replace`
  is_replace = dplyr::coalesce(stringi::stri_detect_fixed(options_str, "replace"), FALSE)
  gen_vars = NA_character_
  if (!is_replace) {
      gen_opt_match = stringi::stri_match_first_regex(options_str, "\\bgenerate\\s*\\(([^)]+)\\)")
      if (!is.na(gen_opt_match[1,1])) {
           gen_vars = stringi::stri_trim_both(gen_opt_match[1,2])
       }
  }

  new_vars = NULL
  if (!is_replace) {
      if (is.na(gen_vars)) {
           return(paste0("# destring requires generate() option or replace option: ", rest_of_cmd))
      }
      new_vars = stringi::stri_split_regex(gen_vars, "\\s+")[[1]]
      new_vars = new_vars[new_vars != ""]
      if (length(new_vars) != length(vars_to_destring)) {
          return(paste0("# destring generate() option requires same number of new variables as old variables."))
      }
  } else {
      # If replace option, new vars are the same as old vars.
      new_vars = vars_to_destring
  }


  # Translate if/in condition
  r_if_in_cond = NA_character_
  if (!is.na(stata_if_in_cond) && stata_if_in_cond != "") {
       r_if_in_cond = translate_stata_expression_with_r_values(stata_if_in_cond, line_num, cmd_df, context = list(is_by_group=FALSE))
       if (is.na(r_if_in_cond) || r_if_in_cond == "") {
           return(paste0("# Failed to translate if/in condition for destring: ", stata_if_in_cond))
       }
  }

  # R equivalent: readr::parse_number() or as.numeric()
  # readr::parse_number is better at handling non-numeric parts and commas/$, results in NA on failure.
  # as.numeric() often results in warnings/errors or NA on failure.
  # Stata destring option `ignore()` can specify characters to ignore. `force` allows unconvertible values to become missing.
  # `readr::parse_number` handles some of this.

  mutate_exprs = character(length(vars_to_destring))
  for (k in seq_along(vars_to_destring)) {
      old_var = vars_to_destring[k]
      new_var = new_vars[k]
      source_var_r = old_var # R variable name for the source column

      # Calculate destrung values
      # Using readr::parse_number
      destrung_value_expr = paste0("readr::parse_number(as.character(", source_var_r, "))")
      # as.character needed in case the variable is factor/labelled etc.

      # Apply the if/in condition for replacement
      if (!is.na(r_if_in_cond) && r_if_in_cond != "") {
          # For rows meeting condition, use destrung value. Otherwise, keep original (or NA if new var).
           if (is_replace) {
               # Replace in place: use destrung if condition, old value otherwise
               final_value_expr = paste0("dplyr::if_else((dplyr::coalesce(as.numeric(", r_if_in_cond, "), 0) != 0), ", destrung_value_expr, ", `", source_var_r, "`)")
           } else {
               # Generate new var: use destrung if condition, NA otherwise
               final_value_expr = paste0("dplyr::if_else((dplyr::coalesce(as.numeric(", r_if_in_cond, "), 0) != 0), ", destrung_value_expr, ", NA_real_)") # Assuming numeric result
           }
      } else {
          # Apply to all rows
           final_value_expr = destrung_value_expr
      }

      mutate_exprs[k] = paste0("`", new_var, "` = ", final_value_expr)
  }

   # Combine mutate expressions
  mutate_exprs_str = paste(mutate_exprs, collapse = ",\n  ")

  # Build the final R code using dplyr::mutate
  r_code_lines = c(paste0("data = dplyr::mutate(data, ", mutate_exprs_str, ")")) # Changed to dplyr::mutate


  r_code_str = paste(r_code_lines, collapse="\n")

  # Add comment about options if any were present but not handled (excluding generate/replace)
   options_str_cleaned = options_str
   # Remove generate() or replace from options string
   if (is_replace) {
       options_str_cleaned = stringi::stri_replace_first_fixed(options_str_cleaned, "replace", "")
   } else {
       options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "\\bgenerate\\s*\\([^)]+\\)", "")
   }
   options_str_cleaned = stringi::stri_trim_both(stringi::stri_replace_all_regex(options_str_cleaned, ",+", ",")) # Clean up multiple commas
   options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "^,+", "") # Remove leading comma


   if (!is.na(options_str_cleaned) && options_str_cleaned != "") {
        r_code_str = paste0(r_code_str, paste0(" # Other options ignored: ", options_str_cleaned))
   }


  return(r_code_str)
}

