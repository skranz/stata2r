# FILE: R/t_recode.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_recode = function(rest_of_cmd) {
  restore.point("s2r_p_recode")
  parts = stringi::stri_split_fixed(stringi::stri_trim_both(rest_of_cmd), "(", n=2)[[1]]
  if (length(parts) != 2) return(list(varlist=NA_character_))

  varlist = stringi::stri_trim_both(parts[1])
  rules_rest = paste0("(", parts[2])

  parsed = s2r_parse_if_in(rules_rest)

  options_match = stringi::stri_match_first_regex(parsed$base_str, ",\\s*(.*)$")
  options_str = NA_character_
  rules_str = parsed$base_str
  if (!is.na(options_match[1,1])) {
    options_str = stringi::stri_trim_both(options_match[1,2])
    rules_str = stringi::stri_trim_both(stringi::stri_replace_last_regex(parsed$base_str, ",\\s*(.*)$", ""))
  }

  gen_vars = NA_character_
  if (!is.na(options_str)) {
    gen_opt = stringi::stri_match_first_regex(options_str, "\\bgen\\s*\\(([^)]+)\\)")
    if (!is.na(gen_opt[1,1])) gen_vars = stringi::stri_trim_both(gen_opt[1,2])
  }

  rule_matches = stringi::stri_match_all_regex(rules_str, "\\(([^)]*)\\)")[[1]]
  recode_rules_raw = if(NROW(rule_matches) > 0) rule_matches[,2] else character(0)

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
  } else if (old_part_raw == "missing" || dplyr::coalesce(stringi::stri_detect_regex(old_part_raw, "^\\.\\w?$"), FALSE)) {
    r_condition = "sfun_missing(.VAR.)"
  } else if (old_part_raw == "nonmissing") {
    r_condition = "!sfun_missing(.VAR.)"
  } else if (grepl("\\s+thru\\s+", old_part_raw)) {
    range_parts = stringi::stri_split_regex(old_part_raw, "\\s+thru\\s+", n=2)[[1]]
    val1 = translate_stata_expression_to_r(stringi::stri_trim_both(range_parts[1]))
    val2 = translate_stata_expression_to_r(stringi::stri_trim_both(range_parts[2]))
    r_condition = paste0(".VAR. >= ", val1, " & .VAR. <= ", val2)
  } else if (grepl("/", old_part_raw)) {
    range_parts = stringi::stri_split_regex(old_part_raw, "/", n=2)[[1]]
    val1 = translate_stata_expression_to_r(stringi::stri_trim_both(range_parts[1]))
    val2 = translate_stata_expression_to_r(stringi::stri_trim_both(range_parts[2]))
    r_condition = paste0(".VAR. >= ", val1, " & .VAR. <= ", val2)
  } else {
    old_values = stringi::stri_split_regex(old_part_raw, "\\s+")[[1]]
    old_values = old_values[!is.na(old_values) & old_values != ""]
    r_values = sapply(old_values, function(val) {
      if (is.na(val) || val == "." || dplyr::coalesce(stringi::stri_detect_regex(val, "^\\.[a-zA-Z]$"), FALSE)) return("NA_real_")
      translate_stata_expression_to_r(val)
    })
    r_condition = paste0(".VAR. %in% c(", paste(r_values, collapse = ", "), ")")
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
        else if (!dplyr::coalesce(stringi::stri_startswith_fixed(r_new_value, '"'), FALSE) && !dplyr::coalesce(stringi::stri_startswith_fixed(r_new_value, "'"), FALSE)) {
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

    if ((dplyr::coalesce(stringi::stri_startswith_fixed(new_part_raw, '"'), FALSE)) ||
        (dplyr::coalesce(stringi::stri_startswith_fixed(new_part_raw, "'"), FALSE))) {
      any_rule_implies_string_output = TRUE
    }

    label_match = stringi::stri_match_first_regex(new_part_raw, "^\\s*([^\\s]+)\\s+(?:\"([^\"]*)\"|'([^']*)')\\s*$")
    if (!is.na(label_match[1,1])) {
      any_rule_implies_labelled_numeric_output = TRUE
      numeric_val_part = stringi::stri_trim_both(label_match[1,2])
      string_label_part = ifelse(!is.na(label_match[1,3]), label_match[1,3], label_match[1,4])

      r_numeric_val = NA_real_
      if (numeric_val_part != "." && !stringi::stri_detect_regex(numeric_val_part, "^\\.[a-zA-Z]$")) {
        r_numeric_val = as.numeric(numeric_val_part)
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

  r_rules_templates = sapply(parsed$rules, translate_recode_rule_template, final_r_var_type_is_string = final_r_var_type_is_string)

  # IMPORTANT: Map quote_for_r_literal over the vector to prevent truncation
  quoted_rules = sapply(r_rules_templates, quote_for_r_literal)

  args = c("data = data", paste0("varlist_str = ", quote_for_r_literal(parsed$varlist)),
           paste0("rules_templates = c(", paste(quoted_rules, collapse=", "), ")"))
  if (!is.na(parsed$gen_vars)) args = c(args, paste0("gen_vars_str = ", quote_for_r_literal(parsed$gen_vars)))
  if (!is.na(r_subset_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_subset_cond)))
  args = c(args, paste0("is_string = ", final_r_var_type_is_string))

  if (final_r_var_type_is_labelled_numeric && length(final_labels_map) > 0) {
    labels_vector_r_code = paste0("stats::setNames(c(", paste0(unname(final_labels_map), collapse=", "), "), c(", paste0('"', names(final_labels_map), '"', collapse=", "), "))")
    args = c(args, paste0("labels_map = ", labels_vector_r_code))
  }

  return(paste0("data = scmd_recode(", paste(args, collapse = ", "), ")"))
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_recode = function(data, varlist_str, rules_templates, gen_vars_str = NA_character_, r_if_cond = NA_character_, is_string = FALSE, labels_map = NULL) {
  restore.point("scmd_recode")

  vars_actual = expand_varlist(varlist_str, names(data))

  new_vars = vars_actual
  if (!is.na(gen_vars_str)) {
    new_vars = stringi::stri_split_regex(gen_vars_str, "\\s+")[[1]]
    new_vars = new_vars[new_vars != ""]
    if (length(new_vars) != length(vars_actual)) stop("scmd_recode: gen() requires same number of vars.")
  }

  for (i in seq_along(vars_actual)) {
    old_var = vars_actual[i]
    new_var = new_vars[i]

    old_attrs = attributes(data[[old_var]])

    r_rules = gsub(".VAR.", paste0("`", old_var, "`"), rules_templates, fixed = TRUE)

    # STATA FALLBACK: If values do not match any conditions, they are left unchanged.
    fallback = if (is_string) paste0("as.character(`", old_var, "`)") else paste0("as.numeric(`", old_var, "`)")
    r_rules = c(r_rules, paste0("TRUE ~ ", fallback))

    case_when_expr = paste0("dplyr::case_when(\n    ", paste(r_rules, collapse = ",\n    "), "\n  )")

    if (!is.na(r_if_cond) && r_if_cond != "") {
      final_val_expr = paste0("dplyr::if_else((dplyr::coalesce(as.numeric(", r_if_cond, "), 0) != 0), ", case_when_expr, ", `", old_var, "`)")
    } else {
      final_val_expr = case_when_expr
    }

    if (is_string) {
      final_val_expr = paste0("as.character(", final_val_expr, ")")
    } else {
      final_val_expr = paste0("as.numeric(", final_val_expr, ")")
    }

    eval(parse(text = paste0("data = dplyr::mutate(data, `", new_var, "` = ", final_val_expr, ")")))

    # Restore or Assign Labels
    if (!is.null(labels_map)) {
      data[[new_var]] = haven::labelled(data[[new_var]], labels = labels_map)
    } else if (old_var == new_var && !is_string) {
      # If replacing and no new labels are provided, keep original labels matching Stata
      if (!is.null(old_attrs$labels)) attr(data[[new_var]], "labels") = old_attrs$labels
      if (!is.null(old_attrs$label)) attr(data[[new_var]], "label") = old_attrs$label
      if (!is.null(old_attrs$class) && "haven_labelled" %in% old_attrs$class) {
        class(data[[new_var]]) = old_attrs$class
      }
    }
  }

  return(data)
}
