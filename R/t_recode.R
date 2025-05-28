# Translate Stata 'recode' command
# Stata: recode varlist (rule1)(rule2)... [if] [in] [, options]
# Example: recode income (0=.) (100 thru 200 = 150) (300 301 305 = 300) (else=copy), gen(newincome)

t_recode = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_recode") # Added restore.point
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # Parse varlist, rules, if/in, options (especially gen())
  # This parsing is quite complex due to nested parentheses and optional parts.
  # Let's try to split varlist first, then find rules in parentheses.
  # Example: `recode var1 var2 (0=.) (1/10=1) (else=copy) if cond, gen(new1 new2)`

  # Split at first parenthesis to get varlist
  parts_varlist = stringi::stri_split_fixed(rest_of_cmd_trimmed, "(", n=2)[[1]]
  if (length(parts_varlist) != 2) {
      return(paste0("# Failed to parse recode command (missing rules): ", rest_of_cmd))
  }
  varlist_str = stringi::stri_trim_both(parts_varlist[1])
  vars_to_recode = stringi::stri_split_regex(varlist_str, "\\s+")[[1]]
  vars_to_recode = vars_to_recode[vars_to_recode != ""]

  if (length(vars_to_recode) == 0) {
      return(paste0("# recode command requires varlist: ", rest_of_cmd))
  }

  rules_and_rest = paste0("(", parts_varlist[2]) # Put the '(' back

  # Separate if/in and options from rules
  stata_if_in_cond = NA_character_
  options_str = NA_character_

  # Look for `if` or `in` first
  if_in_match = stringi::stri_match_first_regex(rules_and_rest, "\\s+(?:if\\s+|in\\s+)(.*)$")
  if(!is.na(if_in_match[1,1])) {
      stata_if_in_cond = if_in_match[1,2]
      rules_and_rest = stringi::stri_replace_last_regex(rules_and_rest, "\\s+(?:if\\s+|in\\s+)(.*)$", "")
      rules_and_rest = stringi::stri_trim_both(rules_and_rest)
  }

  # Look for options after the if/in part or after rules
  options_match = stringi::stri_match_first_regex(rules_and_rest, ",\\s*(.*)$")
  if (!is.na(options_match[1,1])) {
      options_str = stringi::stri_trim_both(options_match[1,2])
      rules_part = stringi::stri_replace_last_regex(rules_and_rest, ",\\s*(.*)$", "")
      rules_part = stringi::stri_trim_both(rules_part)
  } else {
      rules_part = rules_and_rest
  }

  # Now parse the rules part: (rule1)(rule2)...
  # Find all rules within parentheses
  rule_matches = stringi::stri_match_all_regex(rules_part, "\\(([^)]*)\\)")[[1]] # Match content inside ()
  if (NROW(rule_matches) == 0) {
      return(paste0("# Failed to parse recode rules: ", rules_part))
  }
  recode_rules_raw = rule_matches[,2] # Extract the content within parentheses


  # Parse options, specifically `gen()`
  gen_vars = NA_character_
  if (!is.na(options_str)) {
      gen_opt_match = stringi::stri_match_first_regex(options_str, "\\bgen\\s*\\(([^)]+)\\)")
      if (!is.na(gen_opt_match[1,1])) {
           gen_vars = stringi::stri_trim_both(gen_opt_match[1,2])
       }
  }

  new_vars = NULL
  if (!is.na(gen_vars)) {
      new_vars = stringi::stri_split_regex(gen_vars, "\\s+")[[1]]
      new_vars = new_vars[new_vars != ""]
      if (length(new_vars) != length(vars_to_recode)) {
          return(paste0("# recode gen() option requires same number of new variables as old variables."))
      }
  } else {
      # If no gen() option, recode is done in place. New vars are the same as old vars.
      new_vars = vars_to_recode
  }

  # Determine if the target variable(s) should be string type
  any_rule_implies_string = FALSE
  for (rule_raw in recode_rules_raw) {
      rule_str_trimmed = stringi::stri_trim_both(rule_raw)
      parts_eq = stringi::stri_split_fixed(rule_str_trimmed, "=", n=2)[[1]]
      if (length(parts_eq) != 2) next # Skip malformed rules
      new_part_raw = stringi::stri_trim_both(parts_eq[2])

      # A rule implies string type if:
      # 1. The new value is a string literal (e.g., "new_string_value").
      # 2. The new value is a numeric value with an attached string label (e.g., 1 "label").
      if ( (stringi::stri_startswith_fixed(new_part_raw, '"') && stringi::stri_endswith_fixed(new_part_raw, '"')) ||
           (stringi::stri_startswith_fixed(new_part_raw, "'") && stringi::stri_endswith_fixed(new_part_raw, "'")) ) {
          any_rule_implies_string = TRUE
          break
      }
      # Check for 'value "label"' syntax
      label_match = stringi::stri_match_first_regex(new_part_raw, "^\\s*(-?\\d*\\.?\\d+e?-?\\d*|-?\\.\\w?|\\S+)\\s+(?:\"([^\"]*)\"|'([^']*)')\\s*$")
      if (!is.na(label_match[1,1])) {
          any_rule_implies_string = TRUE
          break
      }
  }


  # Translate the if/in condition for subsetting
  r_subset_cond = NA_character_
  data_source_for_recode = "data"
  r_code_prefix = "" # Code to create subset if needed

  if (!is.na(stata_if_in_cond) && stata_if_in_cond != "") {
      # Stata recode applies if/in to select observations *to be recoded*.
      # Observations not meeting if/in condition are left unchanged.
      # This means the R code needs to apply the recoding conditionally.
      r_subset_cond = translate_stata_expression_with_r_values(stata_if_in_cond, line_num, cmd_df, context = list(is_by_group = FALSE))
      if (is.na(r_subset_cond) || r_subset_cond == "") {
           return(paste0("# Failed to translate if/in condition for recode: ", stata_if_in_cond))
       }
      # The actual recoding logic (case_when/ifelse) will incorporate this condition.
      # No need for a separate subset dataframe here.
  }


  # Translate rules into R case_when clauses
  # Rule format: old_value = new_value
  # old_value can be: single value, range (val1/val2, val1 thru val2), list (val1 val2), else, missing (.)
  # new_value can be: single value, copy (use original value), missing (.)

  translate_recode_rule = function(rule_str, source_var_r, target_is_string) {
      rule_str = stringi::stri_trim_both(rule_str)
      parts_eq = stringi::stri_split_fixed(rule_str, "=", n=2)[[1]]
      if (length(parts_eq) != 2) {
          return(paste0("## Error parsing rule: ", rule_str))
      }
      old_part_raw = stringi::stri_trim_both(parts_eq[1])
      new_part_raw = stringi::stri_trim_both(parts_eq[2])

      # Translate old_part into R condition (left side of case_when ~ )
      r_condition = ""
      if (old_part_raw == "else") {
          r_condition = "TRUE" # This rule is the fallback
      } else if (old_part_raw == "missing") {
           r_condition = paste0("is.na(", source_var_r, ")") # Missing value rule
      } else if (old_part_raw == "nonmissing") {
           r_condition = paste0("!is.na(", source_var_r, ")") # Non-missing value rule
      } else if (grepl("\\s+thru\\s+", old_part_raw)) {
           # Range: val1 thru val2
           range_parts = stringi::stri_split_regex(old_part_raw, "\\s+thru\\s+", n=2)[[1]]
           val1 = translate_stata_expression_to_r(stringi::stri_trim_both(range_parts[1]), context=list(is_by_group=FALSE))
           val2 = translate_stata_expression_to_r(stringi::stri_trim_both(range_parts[2]), context=list(is_by_group=FALSE))
           if (is.na(val1) || is.na(val2)) return(paste0("## Error translating range values in rule: ", rule_str))
           r_condition = paste0(source_var_r, " >= ", val1, " & ", source_var_r, " <= ", val2)
      } else if (grepl("/", old_part_raw)) {
          # Range: val1/val2
           range_parts = stringi::stri_split_regex(old_part_raw, "/", n=2)[[1]]
           val1 = translate_stata_expression_to_r(stringi::stri_trim_both(range_parts[1]), context=list(is_by_group=FALSE))
           val2 = translate_stata_expression_to_r(stringi::stri_trim_both(range_parts[2]), context=list(is_by_group=FALSE))
           if (is.na(val1) || is.na(val2)) return(paste0("## Error translating range values in rule: ", rule_str))
           r_condition = paste0(source_var_r, " >= ", val1, " & ", source_var_r, " <= ", val2) # Stata / is inclusive range
      }
      else {
          # List of values or single value
          old_values = stringi::stri_split_regex(old_part_raw, "\\s+")[[1]]
          old_values = old_values[old_values != ""]
          r_values = sapply(old_values, function(val) {
               # Use translate_stata_expression_to_r for each value in the list
               translate_stata_expression_to_r(val, context=list(is_by_group=FALSE))
          })
          r_values = r_values[!is.na(r_values)] # Filter out any NA from translation for safety
          if (length(r_values) == 0) return(paste0("## Error translating old values in rule: ", rule_str))
          r_condition = paste0(source_var_r, " %in% c(", paste(r_values, collapse = ", "), ")")
      }


      # Translate new_part into R value (right side of case_when ~ )
      r_new_value = ""
      if (new_part_raw == "copy") {
          r_new_value = source_var_r # Use the original variable value
      } else {
          # Check for numeric value with optional label, e.g., '1 "Very Low"'
          label_match = stringi::stri_match_first_regex(new_part_raw, "^\\s*(-?\\d*\\.?\\d+e?-?\\d*|-?\\.\\w?|\\S+)\\s+(?:\"([^\"]*)\"|'([^']*)')\\s*$")
          if (!is.na(label_match[1,1])) {
              # It's a "value label" syntax
              string_label_part = ifelse(!is.na(label_match[1,3]), label_match[1,3], label_match[1,4])
              numeric_val_part = stringi::stri_trim_both(label_match[1,2])

              if (target_is_string) {
                  r_new_value = quote_for_r_literal(string_label_part) # Return the label as a string literal
              } else {
                  # If target is numeric, return the numeric value, translated
                  r_new_value = translate_stata_expression_to_r(numeric_val_part, context=list(is_by_group=FALSE))
              }
          } else {
              # It's a plain expression or literal (numeric or string)
              r_new_value = translate_stata_expression_to_r(new_part_raw, context=list(is_by_group=FALSE))
          }
      }

      # Final adjustment for NA type based on target_is_string
      if (target_is_string) {
          # If the target is a string, any NA result should be NA_character_
          # This covers cases like `missing = .` or `else = .` when the variable is string
          if (r_new_value == "NA_real_") { # Check if the string literal "NA_real_" was produced
              r_new_value = "NA_character_"
          }
      }

      return(paste0(r_condition, " ~ ", r_new_value))
  }

  # Generate case_when expression for each variable
  mutate_exprs = character(length(vars_to_recode))
  for (k in seq_along(vars_to_recode)) {
      old_var = vars_to_recode[k]
      new_var = new_vars[k]
      source_var_r = old_var # R variable name for the source column

      # Translate all rules for this variable
      r_rules = sapply(recode_rules_raw, translate_recode_rule, source_var_r = source_var_r, target_is_string = any_rule_implies_string)

      # Combine rules into a case_when statement
      case_when_expr = paste0("dplyr::case_when(\n    ", paste(r_rules, collapse = ",\n    "), "\n  )")

      # Apply global if/in condition around the case_when
      if (!is.na(r_subset_cond) && r_subset_cond != "") {
          # If condition is met, apply case_when. Otherwise, keep original value.
          # Stata's `if` condition treats missing as FALSE, so use coalesce.
          final_value_expr = paste0("dplyr::if_else(dplyr::coalesce(", r_subset_cond, ", FALSE),\n",
                                    "    ", case_when_expr, ",\n",
                                    "    `", source_var_r, "`)") # Keep original value if condition not met.
      } else {
          final_value_expr = case_when_expr
      }


      mutate_exprs[k] = paste0("`", new_var, "` = ", final_value_expr)
  }

  # Combine mutate expressions
  mutate_exprs_str = paste(mutate_exprs, collapse = ",\n  ")

  # Build the final R code using dplyr::mutate
  r_code_lines = c(paste0("data = dplyr::mutate(data, ", mutate_exprs_str, ")")) # Changed to dplyr::mutate


  r_code_str = paste(r_code_lines, collapse="\n")

  # Add comment about options if any were present but not handled (excluding gen)
   options_str_cleaned = options_str
   if (!is.na(options_str_cleaned)) {
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "\\bgen\\s*\\([^)]+\\)", "")
        options_str_cleaned = stringi::stri_trim_both(stringi::stri_replace_all_regex(options_str_cleaned, ",+", ",")) # Clean up multiple commas
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "^,+", "") # Remove leading comma
   }

   if (!is.na(options_str_cleaned) && options_str_cleaned != "") {
        r_code_str = paste0(r_code_str, paste0("\n# Other options ignored: ", options_str_cleaned))
   }


  return(r_code_str)
}

