```markdown
!MODIFICATION t_recode.R
scope = "file"
file = "R/t_recode.R"
is_new_file = false
description = "Update recode parsing to handle rules without parentheses and complex LHS."
---
```r
# FILE: R/t_recode.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_recode = function(rest_of_cmd) {
  restore.point("s2r_p_recode")

  parsed = s2r_parse_if_in(rest_of_cmd)
  base_str = stringi::stri_trim_both(parsed$base_str)
  
  options_str = NA_character_
  rules_str = base_str
  
  comma_split = stringi::stri_split_fixed(base_str, ",")[[1]]
  if (length(comma_split) > 1) {
      last_part = stringi::stri_trim_both(comma_split[length(comma_split)])
      if (grepl("^(gen|generate|prefix|test|replace)\\b", last_part)) {
          options_str = last_part
          rules_str = paste(comma_split[-length(comma_split)], collapse = ",")
      } else if (length(comma_split) == 2) {
          options_str = last_part
          rules_str = comma_split[1]
      }
  }

  gen_vars = NA_character_
  if (!is.na(options_str)) {
    gen_opt = stringi::stri_match_first_regex(options_str, "\\b(?:gen|generate)\\s*\\(([^)]+)\\)")
    if (!is.na(gen_opt[1,1])) gen_vars = stringi::stri_trim_both(gen_opt[1,2])
  }
  
  varlist = NA_character_
  recode_rules_raw = character(0)
  
  first_paren = stringi::stri_locate_first_fixed(rules_str, "(")[1,1]
  first_eq = stringi::stri_locate_first_fixed(rules_str, "=")[1,1]
  
  if (!is.na(first_paren) && (is.na(first_eq) || first_paren < first_eq)) {
      varlist = stringi::stri_trim_both(stringi::stri_sub(rules_str, 1, first_paren - 1))
      rules_part = stringi::stri_sub(rules_str, first_paren)
      rule_matches = stringi::stri_match_all_regex(rules_part, "\\(([^)]+)\\)")[[1]]
      if (NROW(rule_matches) > 0) {
          recode_rules_raw = rule_matches[,2]
      }
  } else if (!is.na(first_eq)) {
      parts = stringi::stri_split_fixed(rules_str, "=")[[1]]
      tokens1 = stringi::stri_split_regex(stringi::stri_trim_both(parts[1]), "\\s+")[[1]]
      
      is_value_token = function(tok) {
          grepl("^[0-9\\.\\-]|^(min|max|missing|nonmissing|else)$|/|thru", tok, ignore.case = TRUE)
      }
      
      lhs1_tokens = character(0)
      idx = length(tokens1)
      while (idx > 1 && is_value_token(tokens1[idx])) {
          lhs1_tokens = c(tokens1[idx], lhs1_tokens)
          idx = idx - 1
      }
      
      if (idx == 0) {
          varlist = ""
      } else {
          varlist = paste(tokens1[1:idx], collapse = " ")
      }
      
      LHS_list = list(paste(lhs1_tokens, collapse = " "))
      RHS_list = list()
      
      for (i in 2:length(parts)) {
          part_str = stringi::stri_trim_both(parts[i])
          
          if (i == length(parts)) {
              RHS_list[[i-1]] = part_str
          } else {
              val_match = stringi::stri_match_first_regex(part_str, "^([^\\s\"']+|\\([^)]+\\))\\s*")
              if (!is.na(val_match[1,1])) {
                  rhs_val = val_match[1,2]
                  remainder = stringi::stri_trim_both(stringi::stri_sub(part_str, stringi::stri_length(val_match[1,1]) + 1))
                  
                  lbl_match = stringi::stri_match_first_regex(remainder, "^(\"[^\"]*\"|'[^']*')\\s*")
                  if (!is.na(lbl_match[1,1])) {
                      rhs_lbl = lbl_match[1,2]
                      RHS_list[[i-1]] = paste(rhs_val, rhs_lbl)
                      LHS_list[[i]] = stringi::stri_trim_both(stringi::stri_sub(remainder, stringi::stri_length(lbl_match[1,1]) + 1))
                  } else {
                      RHS_list[[i-1]] = rhs_val
                      LHS_list[[i]] = remainder
                  }
              } else {
                 RHS_list[[i-1]] = part_str
                 LHS_list[[i]] = ""
              }
          }
      }
      
      for (j in seq_along(RHS_list)) {
          recode_rules_raw = c(recode_rules_raw, paste(LHS_list[[j]], "=", RHS_list[[j]]))
      }
  }
  
  list(varlist = varlist, rules = recode_rules_raw, if_str = parsed$if_str, in_str = parsed$in_str, gen_vars = gen_vars)
}

# Helper to translate individual rules (returns generic templates using .VAR.)
translate_recode_rule_template = function(rule_str, final_r_var_type_is_string) {
  restore.point("translate_recode_rule_template")
  rule_str = stringi::stri_trim_both(rule_str)
  parts_eq = stringi::stri_split_fixed(rule_str, "=", n=2)[[1]]
  if (length(parts_eq) != 2) return(paste0("## Error parsing rule: ", rule_str))

  old_part_raw = stringi::stri_trim_both(parts_eq[1])
  new_part_raw = stringi::stri_trim_both(parts_eq[2])

  # Condition Side
  r_condition = ""
  if (old_part_raw == "else") {
    r_condition = "TRUE"
  } else {
    old_part_raw = stringi::stri_replace_all_regex(old_part_raw, "(?i)\\s+thru\\s+", "/")
    old_values = stringi::stri_split_regex(old_part_raw, "\\s+")[[1]]
    old_values = old_values[!is.na(old_values) & old_values != ""]
    
    cond_parts = character(0)
    exact_vals = character(0)
    
    for (val in old_values) {
      if (val == "missing" || fast_coalesce(stringi::stri_detect_regex(val, "^\\.\\w?$"), FALSE)) {
        cond_parts = c(cond_parts, "sfun_missing(.VAR.)")
      } else if (val == "nonmissing") {
        cond_parts = c(cond_parts, "!sfun_missing(.VAR.)")
      } else if (grepl("/", val)) {
        range_parts = stringi::stri_split_fixed(val, "/", n=2)[[1]]
        v1 = stringi::stri_trim_both(range_parts[1])
        v2 = stringi::stri_trim_both(range_parts[2])
        val1 = if (tolower(v1) == "min") "-Inf" else translate_stata_expression_to_r(v1)
        val2 = if (tolower(v2) == "max") "Inf" else translate_stata_expression_to_r(v2)
        cond_parts = c(cond_parts, paste0("(.VAR. >= ", val1, " & .VAR. <= ", val2, ")"))
      } else {
        exact_vals = c(exact_vals, translate_stata_expression_to_r(val))
      }
    }
    
    if (length(exact_vals) > 0) {
      cond_parts = c(cond_parts, paste0(".VAR. %in% c(", paste(exact_vals, collapse = ", "), ")"))
    }
    
    r_condition = paste(cond_parts, collapse = " | ")
  }

  # Value Side
  r_new_value = ""
  if (new_part_raw == "copy") {
    r_new_value = ".VAR."
    if (final_r_var_type_is_string) r_new_value = paste0("as.character(", r_new_value, ")")
  } else {
    label_match = stringi::stri_match_first_regex(new_part_raw, "^\\s*([^\\s]+)\\s+(?:\"([^\"]*)\"|'([^']*)')\\s*$")
    if (!is.na(label_match[1,1])) {
      if (final_r_var_type_is_string) {
        string_label_part = ifelse(!is.na(label_match[1,3]), label_match[1,3], label_match[1,4])
        r_new_value = quote_for_r_literal(string_label_part)
      } else {
        r_new_value = translate_stata_expression_to_r(stringi::stri_trim_both(label_match[1,2]))
      }
    } else {
      r_new_value = translate_stata_expression_to_r(new_part_raw)
      if (final_r_var_type_is_string) {
        if (r_new_value == "NA_real_") r_new_value = '""'
        else if (!fast_coalesce(stringi::stri_startswith_fixed(r_new_value, '"'), FALSE) && !fast_coalesce(stringi::stri_startswith_fixed(r_new_value, "'"), FALSE)) {
          r_new_value = paste0("as.character(", r_new_value, ")")
        }
      }
    }
  }

  return(paste0(r_condition, " ~ ", r_new_value))
}

# 2. Code Generation Phase: Emit R code
t_recode = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_recode")
  parsed = s2r_p_recode(rest_of_cmd)
  if (is.na(parsed$varlist) || length(parsed$rules) == 0) return(paste0("# Failed to parse recode command: ", rest_of_cmd))

  any_rule_implies_string_output = FALSE
  any_rule_implies_labelled_numeric_output = FALSE
  collected_labels = list()

  for (rule_raw in parsed$rules) {
    parts_eq = stringi::stri_split_fixed(stringi::stri_trim_both(rule_raw), "=", n=2)[[1]]
    if (length(parts_eq) != 2) next
    new_part_raw = stringi::stri_trim_both(parts_eq[2])

    if ((fast_coalesce(stringi::stri_startswith_fixed(new_part_raw, '"'), FALSE)) ||
        (fast_coalesce(stringi::stri_startswith_fixed(new_part_raw, "'"), FALSE))) {
      any_rule_implies_string_output = TRUE
    }

    label_match = stringi::stri_match_first_regex(new_part_raw, "^\\s*([^\\s]+)\\s+(?:\"([^\"]*)\"|'([^']*)')\\s*$")
    if (!is.na(label_match[1,1])) {
      any_rule_implies_labelled_numeric_output = TRUE
      numeric_val_part = stringi::stri_trim_both(label_match[1,2])
      string_label_part = ifelse(!is.na(label_match[1,3]), label_match[1,3], label_match[1,4])

      r_numeric_val = NA_real_
      if (numeric_val_part != "." && !stringi::stri_detect_regex(numeric_val_part, "^\\.[a-zA-Z]$")) {
        r_numeric_val = suppressWarnings(as.numeric(numeric_val_part))
      }
      if (!is.na(r_numeric_val) && !is.na(string_label_part)) {
        collected_labels[[length(collected_labels) + 1]] = list(label = string_label_part, value = r_numeric_val)
      }
    }
  }

  final_r_var_type_is_string = any_rule_implies_string_output
  final_r_var_type_is_labelled_numeric = !final_r_var_type_is_string && any_rule_implies_labelled_numeric_output

  final_labels_map = list()
  if (final_r_var_type_is_labelled_numeric && length(collected_labels) > 0) {
    temp_df_labels = data.frame(
        label = sapply(collected_labels, `[[`, "label"),
        value = sapply(collected_labels, `[[`, "value"),
        stringsAsFactors = FALSE
    )
    temp_df_labels$original_order = seq_len(NROW(temp_df_labels))
    temp_df_labels = temp_df_labels[order(temp_df_labels$value, -temp_df_labels$original_order), ]
    temp_df_labels = temp_df_labels[!duplicated(temp_df_labels$value, fromLast = TRUE), ]
    temp_df_labels = temp_df_labels[order(temp_df_labels$value), ]

    final_labels_map = stats::setNames(temp_df_labels$value, temp_df_labels$label)
  }

  r_subset_cond = NA_character_
  if (!is.na(parsed$if_str)) r_subset_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, list(is_by_group = FALSE))

  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  r_rules_templates = sapply(parsed$rules, translate_recode_rule_template, final_r_var_type_is_string = final_r_var_type_is_string)
  quoted_rules = sapply(r_rules_templates, quote_for_r_literal)

  args = c("data = data", paste0("varlist_str = ", quote_for_r_literal(parsed$varlist)),
           paste0("rules_templates = c(", paste(quoted_rules, collapse=", "), ")"))
  if (!is.na(parsed$gen_vars)) args = c(args, paste0("gen_vars_str = ", quote_for_r_literal(parsed$gen_vars)))
  if (!is.na(r_subset_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_subset_cond)))
  if (!is.na(r_in_range)) args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))
  args = c(args, paste0("is_string = ", final_r_var_type_is_string))

  if (final_r_var_type_is_labelled_numeric && length(final_labels_map) > 0) {
    labels_vector_r_code = paste0("stats::setNames(c(", paste0(unname(final_labels_map), collapse=", "), "), c(", paste0('"', names(final_labels_map), '"', collapse=", "), "))")
    args = c(args, paste0("labels_map = ", labels_vector_r_code))
  }

  return(paste0("data = scmd_recode(", paste(args, collapse = ", "), ")"))
}

# 3. Runtime Execution Phase
scmd_recode = function(data, varlist_str, rules_templates, gen_vars_str = NA_character_, r_if_cond = NA_character_, r_in_range = NA_character_, is_string = FALSE, labels_map = NULL) {
  restore.point("scmd_recode")

  vars_actual = expand_varlist(varlist_str, names(data))

  new_vars = vars_actual
  if (!is.na(gen_vars_str)) {
    new_vars = stringi::stri_split_regex(gen_vars_str, "\\s+")[[1]]
    new_vars = new_vars[new_vars != ""]
    if (length(new_vars) != length(vars_actual)) stop("scmd_recode: gen() requires same number of vars.")
  }

  r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))

  mask_expr = ".stata_temp_mask"
  if (!is.na(r_if_cond) && r_if_cond != "") {
    mask_expr = paste0("(.stata_temp_mask & fast_coalesce(as.numeric(", r_if_cond, "), 0) != 0)")
  }

  # Compute in-range mask globally
  in_mask = rep(TRUE, nrow(data))
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask_vec = rep(FALSE, nrow(data))
    in_mask_vec[idx] = TRUE
    in_mask = in_mask_vec
  }
  data$.stata_temp_mask = in_mask

  for (i in seq_along(vars_actual)) {
    old_var = vars_actual[i]
    new_var = new_vars[i]

    # Dynamically capture character targets to prevent NA string coercion
    target_is_char = is.character(data[[old_var]])
    current_is_string = is_string || target_is_char

    old_attrs = attributes(data[[old_var]])

    r_rules = gsub(".VAR.", paste0("`", old_var, "`"), rules_templates, fixed = TRUE)
    # Also resolve abbreviations in rules
    r_rules = vapply(r_rules, function(r) resolve_abbrevs_in_expr(r, names(data)), character(1))

    # STATA FALLBACK: If values do not match any conditions, they are left unchanged.
    fallback = if (current_is_string) paste0("as.character(`", old_var, "`)") else paste0("as.numeric(`", old_var, "`)")
    r_rules = c(r_rules, paste0("TRUE ~ ", fallback))

    case_when_expr = paste0("dplyr::case_when(\n    ", paste(r_rules, collapse = ",\n    "), "\n  )")

    if (mask_expr != ".stata_temp_mask" || (!is.na(r_in_range) && r_in_range != "")) {
      final_val_expr = paste0("dplyr::if_else(", mask_expr, ", ", case_when_expr, ", `", old_var, "`)")
    } else {
      final_val_expr = case_when_expr
    }

    if (current_is_string) {
      final_val_expr = paste0("as.character(", final_val_expr, ")")
    } else {
      final_val_expr = paste0("as.numeric(", final_val_expr, ")")
    }

    data = eval(parse(text = paste0("dplyr::mutate(data, `", new_var, "` = ", final_val_expr, ")")), envir = list(data = data), enclos = parent.frame())

    # Restore or Assign Labels
    if (!is.null(labels_map)) {
      data[[new_var]] = haven::labelled(data[[new_var]], labels = labels_map)
    } else if (old_var == new_var && !current_is_string) {
      # If replacing and no new labels are provided, keep original labels matching Stata
      if (!is.null(old_attrs$labels)) attr(data[[new_var]], "labels") = old_attrs$labels
      if (!is.null(old_attrs$label)) attr(data[[new_var]], "label") = old_attrs$label
      if (!is.null(old_attrs$class) && "haven_labelled" %in% old_attrs$class) {
        class(data[[new_var]]) = old_attrs$class
      }
    }
  }

  data$.stata_temp_mask = NULL
  return(data)
}
```
!END_MODIFICATION t_recode.R
```
