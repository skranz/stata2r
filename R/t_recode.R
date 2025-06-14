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
      options_str = stringi::stri_trim_both(options_match[1,2]) # Corrected to take group 2 for options string
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

  # --- Determine the target variable type and collect labels if applicable ---
  # These flags will track if *any* rule implies a string or labelled numeric output.
  # The final type will be determined based on the presence of these flags.
  any_rule_implies_string_output = FALSE
  any_rule_implies_labelled_numeric_output = FALSE
  collected_labels_for_numeric_target = list() # temp storage for value-label pairs

  for (rule_raw in recode_rules_raw) {
      rule_str_trimmed = stringi::stri_trim_both(rule_raw)
      parts_eq = stringi::stri_split_fixed(rule_str_trimmed, "=", n=2)[[1]]
      if (length(parts_eq) != 2) next # Skip malformed rules
      new_part_raw = stringi::stri_trim_both(parts_eq[2])

      # 1. Check if new value is a plain string literal (e.g., "new_string_value")
      if ( (dplyr::coalesce(stringi::stri_startswith_fixed(new_part_raw, '"') && stringi::stri_endswith_fixed(new_part_raw, '"'), FALSE)) ||
           (dplyr::coalesce(stringi::stri_startswith_fixed(new_part_raw, "'") && stringi::stri_endswith_fixed(new_part_raw, "'"), FALSE)) ) {
          any_rule_implies_string_output = TRUE
      }

      # 2. Check for 'value "label"' syntax (e.g., 1 "Very Low")
      label_match = stringi::stri_match_first_regex(new_part_raw, "^\\s*([^\\s]+)\\s+(?:\"([^\"]*)\"|'([^']*)')\\s*$")
      if (!is.na(label_match[1,1])) {
          any_rule_implies_labelled_numeric_output = TRUE
          numeric_val_part = stringi::stri_trim_both(label_match[1,2])
          string_label_part = ifelse(!is.na(label_match[1,3]), label_match[1,3], label_match[1,4])

          r_numeric_val = NA_real_
          # Robustly handle numeric_val_part, which can be NA from regex if not present.
          numeric_val_part_safe = dplyr::coalesce(numeric_val_part, "") 

          if (numeric_val_part_safe == ".") r_numeric_val = NA_real_ # Stata system missing
          else if (stringi::stri_detect_regex(numeric_val_part_safe, "^\\.[a-zA-Z]$")) r_numeric_val = NA_real_ # Stata extended missing
          else if (numeric_val_part_safe != "") r_numeric_val = as.numeric(numeric_val_part_safe) # Convert numeric strings to numeric
          # If numeric_val_part_safe is "", r_numeric_val remains NA_real_, which is fine.

          if (!is.na(r_numeric_val) && !is.na(string_label_part)) {
            # Use a temporary list to store key-value pairs to handle duplicates
            # Store as c(label, value) and process later.
            collected_labels_for_numeric_target[[length(collected_labels_for_numeric_target) + 1]] = list(label = string_label_part, value = r_numeric_val)
          }
      }
  }

  # Final determination of R type:
  # If any rule implies a string, the variable will be string.
  # Otherwise, if any rule implies a labelled numeric, it will be labelled numeric.
  # Otherwise, it will be plain numeric.
  final_r_var_type_is_string = any_rule_implies_string_output
  final_r_var_type_is_labelled_numeric = !final_r_var_type_is_string && any_rule_implies_labelled_numeric_output

  # Process collected_labels_for_numeric_target if final_r_var_type_is_labelled_numeric is TRUE
  final_labels_map = stats::setNames(numeric(0), character(0)) # Initialize empty named numeric vector
  if (final_r_var_type_is_labelled_numeric && length(collected_labels_for_numeric_target) > 0) {
      temp_df_labels = data.frame(
          label = sapply(collected_labels_for_numeric_target, `[[`, "label"),
          value = sapply(collected_labels_for_numeric_target, `[[`, "value"),
          stringsAsFactors = FALSE
      )
      
      # Stata's recode behavior for labels:
      # If multiple rules map to the same *numeric value*, the label for that value is taken from the *last* rule that defines it.
      # The order of labels in the metadata is typically sorted by value.
      
      # To replicate "last defined label for value": Sort by value (desc), then original_order (desc), then remove duplicates, then re-sort by value (asc).
      temp_df_labels$original_order = seq_len(NROW(temp_df_labels))
      temp_df_labels = temp_df_labels[order(temp_df_labels$value, -temp_df_labels$original_order), ] # Sort by value (asc), then original_order (desc) for tie-breaking
      temp_df_labels = temp_df_labels[!duplicated(temp_df_labels$value, fromLast = TRUE), ] # Keep the last unique for value (which is the last from original order)
      temp_df_labels = temp_df_labels[order(temp_df_labels$value), ] # Final sort by value (asc) for consistent output
      
      final_labels_map = stats::setNames(temp_df_labels$value, temp_df_labels$label)
  }

  # Translate the if/in condition for subsetting
  r_subset_cond = NA_character_
  if (!is.na(stata_if_in_cond) && stata_if_in_cond != "") {
      r_subset_cond = translate_stata_expression_with_r_values(stata_if_in_cond, line_num, cmd_df, context = list(is_by_group = FALSE))
      if (is.na(r_subset_cond) || r_subset_cond == "") {
           return(paste0("# Failed to translate if/in condition for recode: ", stata_if_in_cond))
       }
  }


  # Translate rules into R case_when clauses
  translate_recode_rule = function(rule_str, source_var_r, final_r_var_type_is_string) { 
      restore.point("translate_recode_rule_inner")
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
      } else if (old_part_raw == "missing" || dplyr::coalesce(stringi::stri_detect_regex(old_part_raw, "^\\.\\w?$"), FALSE)) { # Added regex for .a, .b, etc.
           r_condition = paste0("sfun_missing(", source_var_r, ")") # Missing value rule (all Stata missing types to R's NA)
      } else if (old_part_raw == "nonmissing") {
           r_condition = paste0("!sfun_missing(", source_var_r, ")") # Non-missing value rule
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
          old_values = old_values[!is.na(old_values) & old_values != ""] # Filter out NA or empty strings
          r_values = sapply(old_values, function(val) {
               # Use translate_stata_expression_to_r for each value in the list
               # Stata missing values like '.' or '.a' should be handled as NA in R
               # The `if (val == ".")` and `if (stringi::stri_detect_regex(val, "^\\.[a-zA-Z]$"))`
               # need to be robust to `val` being NA.
               if (is.na(val)) return("NA_real_") # Added robustness for NA input to internal function
               if (val == ".") return("NA_real_")
               if (dplyr::coalesce(stringi::stri_detect_regex(val, "^\\.[a-zA-Z]$"), FALSE)) return("NA_real_") # Added coalesce
               translated_val = translate_stata_expression_to_r(val, context=list(is_by_group=FALSE))
               translated_val
          })
          r_values = r_values[!is.na(r_values)] # Filter out any NA from translation for safety
          if (length(r_values) == 0) return(paste0("## Error translating old values in rule: ", rule_str))
          r_condition = paste0(source_var_r, " %in% c(", paste(r_values, collapse = ", "), ")")
      }


      # Translate new_part into R value (right side of case_when ~ )
      r_new_value = ""
      if (new_part_raw == "copy") {
          r_new_value = source_var_r # Use the original variable value
          if (final_r_var_type_is_string) {
            r_new_value = paste0("as.character(", r_new_value, ")")
          }
      } else {
          # Check for numeric value with optional label, e.g., '1 "Very Low"'
          label_match = stringi::stri_match_first_regex(new_part_raw, "^\\s*([^\\s]+)\\s+(?:\"([^\"]*)\"|'([^']*)')\\s*$")
          if (!is.na(label_match[1,1])) {
              # It's a "value label" syntax
              if (final_r_var_type_is_string) {
                  string_label_part = ifelse(!is.na(label_match[1,3]), label_match[1,3], label_match[1,4])
                  r_new_value = quote_for_r_literal(string_label_part) 
              } else {
                  numeric_val_part = stringi::stri_trim_both(label_match[1,2])
                  r_new_value = translate_stata_expression_to_r(numeric_val_part, context=list(is_by_group=FALSE))
              }
          } else {
              # It's a plain expression or literal (numeric or string)
              r_new_value = translate_stata_expression_to_r(new_part_raw, context=list(is_by_group=FALSE))
              if (final_r_var_type_is_string) {
                  if (r_new_value == "NA_real_") {
                      r_new_value = '""' # Stata recode for missing numeric to empty string for string variables
                  } else if (!dplyr::coalesce(stringi::stri_startswith_fixed(r_new_value, '"'), FALSE) && !dplyr::coalesce(stringi::stri_startswith_fixed(r_new_value, "'"), FALSE)) {
                      r_new_value = paste0("as.character(", r_new_value, ")")
                  }
              }
          }
      }

      return(paste0(r_condition, " ~ ", r_new_value))
  }

  # Generate case_when expression for each variable
  mutate_exprs = character(length(vars_to_recode))
  r_code_lines = c()

  for (k in seq_along(vars_to_recode)) {
      old_var = vars_to_recode[k]
      new_var = new_vars[k]
      source_var_r = old_var # R variable name for the source column

      r_rules = sapply(recode_rules_raw, translate_recode_rule, source_var_r = source_var_r, final_r_var_type_is_string = final_r_var_type_is_string)

      case_when_expr = paste0("dplyr::case_when(\n    ", paste(r_rules, collapse = ",\n    "), "\n  )")

      if (!is.na(r_subset_cond) && r_subset_cond != "") {
          final_value_expr = paste0("dplyr::if_else((dplyr::coalesce(as.numeric(", r_subset_cond, "), 0) != 0),\n",
                                    "    ", case_when_expr, ",\n",
                                    "    `", source_var_r, "`)") # Keep original value if condition not met.
      } else {
          final_value_expr = case_when_expr
      }

      if (final_r_var_type_is_string) {
          final_value_expr = paste0("as.character(", final_value_expr, ")")
      } else if (final_r_var_type_is_labelled_numeric) {
          # Changed to as.numeric to match Stata's default float type for new numeric variables
          final_value_expr = paste0("as.numeric(", final_value_expr, ")")
      }


      mutate_exprs[k] = paste0("`", new_var, "` = ", final_value_expr)
  }

  mutate_exprs_str = paste(mutate_exprs, collapse = ",\n  ")

  r_code_lines = c(r_code_lines, paste0("data = dplyr::mutate(data, ", mutate_exprs_str, ")"))

  if (final_r_var_type_is_labelled_numeric && length(final_labels_map) > 0) {
      # Ensure labels are sorted by value for consistent haven::labelled behavior
      sorted_labels = final_labels_map[order(unname(final_labels_map))]
      labels_vector_r_code = paste0("stats::setNames(c(", paste0(unname(sorted_labels), collapse=", "), "), c(", paste0('"', names(sorted_labels), '"', collapse=", "), "))")

      for (new_var in new_vars) {
          r_code_lines = c(r_code_lines, paste0("data[['", new_var, "']] = haven::labelled(data[['", new_var, "']], labels = ", labels_vector_r_code, ")"))
      }
  }


  r_code_str = paste(r_code_lines, collapse="\n")

   options_str_cleaned = options_str
   if (!is.na(options_str_cleaned)) {
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "\\bgen\\s*\\([^)]+\\)", "")
        options_str_cleaned = stringi::stri_trim_both(stringi::stri_replace_all_regex(options_str_cleaned, ",+", ","))
        options_str_cleaned = stringi::stri_replace_first_regex(options_str_cleaned, "^,+", "")
   }

   if (!is.na(options_str_cleaned) && options_str_cleaned != "") {
        r_code_str = paste0(r_code_str, paste0("\n# Other options ignored: ", options_str_cleaned))
   }


  return(r_code_str)
}


