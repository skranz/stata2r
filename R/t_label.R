# Translate Stata 'label' command
# Stata: label define lblname value "label" [value "label" ...] [, add|modify|replace]
# Stata: label values varlist lblname
# Stata: label variable varname "label"

t_label = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_label")
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # Determine subcommand: define, values, variable
  if (stringi::stri_startswith_fixed(rest_of_cmd_trimmed, "define ")) {
    return(t_label_define(rest_of_cmd_trimmed, cmd_obj, cmd_df, line_num))
  } else if (stringi::stri_startswith_fixed(rest_of_cmd_trimmed, "values ")) {
    return(t_label_values(rest_of_cmd_trimmed, cmd_obj, cmd_df, line_num))
  } else if (stringi::stri_startswith_fixed(rest_of_cmd_trimmed, "variable ")) {
    return(t_label_variable(rest_of_cmd_trimmed, cmd_obj, cmd_df, line_num))
  } else {
    return(paste0("# Unknown label subcommand: ", rest_of_cmd))
  }
}

t_label_define = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_label_define")
  # label define lblname value "label" ... [, add|modify|replace]
  # Parts: "define ", lblname, rules, options
  define_match = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*define\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*(.*?)(?:,\\s*(add|modify|replace))?$")
  if (is.na(define_match[1,1])) {
      return(paste0("# Failed to parse label define command: ", rest_of_cmd))
  }
  lblname = define_match[1,2]
  rules_str = stringi::stri_trim_both(define_match[1,3]) # Trim whitespace around rules
  option_type = define_match[1,4] # add, modify, replace, or NA

  # Parse rules: value "label" value "label" ...
  # Find all pairs of (non-space value) and (quoted label)
  rule_matches = stringi::stri_match_all_regex(rules_str, "(-?\\d*\\.?\\d+e?-?\\d*|-?\\.\\w?|\\S+)\\s+(?:\"([^\"]*)\"|'([^']*)')")[[1]]
  # Regex breakdown:
  # (-?\\d*\\.?\\d+e?-?\\d*|-?\\.\\w?|\\S+)  -> Captures numeric values (incl sci notation), Stata missing (.), extended missing (.a), or any non-space sequence for other values.
  # \\s+                                    -> Space separator
  # (?:\"([^\"]*)\"|'([^']*)')              -> Captures label in double OR single quotes. Group 2 for double, Group 3 for single.

  if (NROW(rule_matches) == 0 && rules_str != "") { # Allow empty rules_str if label define is just to create an empty set
      return(paste0("# Failed to parse label define rules: ", rules_str))
  }

  values_from_regex = rule_matches[,2]
  labels_from_regex = ifelse(!is.na(rule_matches[,3]), rule_matches[,3], rule_matches[,4]) # Pick double or single quoted label


  # Convert Stata values (like ".", ".a", numbers) to R numeric or NA_real_
  numeric_values_for_labels = sapply(values_from_regex, function(v) {
      if (v == ".") return(NA_real_) # Stata system missing
      if (stringi::stri_detect_regex(v, "^\\.[a-zA-Z]$")) return(NA_real_) # Stata extended missing
      as.numeric(v) # Convert numeric strings to numeric
  })

  # Construct the R named numeric vector string for haven::labelled format: c("label_string" = value_numeric)
  if (length(labels_from_regex) > 0) {
    label_pairs_for_r_code = paste0('"', labels_from_regex, '" = ', ifelse(is.na(numeric_values_for_labels), "NA_real_", format(numeric_values_for_labels, scientific = FALSE, trim = TRUE)))
    label_map_r_code_str = paste0("stats::setNames(c(", paste(numeric_values_for_labels, collapse=", "), "), c(", paste0('"', labels_from_regex, '"', collapse=", "),"))")
    # Using setNames(values, names) is more robust if names have special chars, though c("name"=value) usually works.
    # The order for setNames is setNames(object, nm). So values first, then names.
    # Corrected: stats::setNames(c(values, names)). No, it's stats::setNames(values, names)
    # The haven::labelled function expects `labels` to be a named numeric vector, e.g. `c("label1" = 1, "label2" = 2)`.
    # So `values` should be the numeric codes, and `names` should be the string labels.
    # The current code: `stats::setNames(c(numeric_values_for_labels), c(labels_from_regex))` seems to do this.
    values_vec_str = paste0("c(", paste(ifelse(is.na(numeric_values_for_labels), "NA_real_", format(numeric_values_for_labels, scientific = FALSE, trim = TRUE)), collapse = ", "), ")")
    names_vec_str = paste0("c(", paste0('"', labels_from_regex, '"', collapse = ", "), ")")
    label_map_r_code_str = paste0("stats::setNames(", values_vec_str, ", ", names_vec_str, ")")

  } else {
    label_map_r_code_str = "stats::setNames(numeric(0), character(0))" # Empty label set
  }


  r_code_lines = c()
  r_code_lines = c(r_code_lines, "if (!exists('stata2r_env$label_defs')) stata2r_env$label_defs = list()")

  # Default behavior if no option is like 'add' but Stata errors if exists. Here, we'll overwrite like 'modify'.
  # 'replace' means remove old definition entirely first. 'modify' and 'add' merge.
  if (is.na(option_type) || option_type %in% c("replace", "modify")) { # Treat no option or modify as overwrite/merge; replace is clear overwrite
      if (is.na(option_type) || option_type == "replace") { # If replace or no option (Stata default is error if exists, we replace)
          r_code_lines = c(r_code_lines, paste0("stata2r_env$label_defs$`", lblname, "` = ", label_map_r_code_str))
      } else { # modify (same as add for this logic: new overwrites old for same value)
          # The logic for combining labels: new labels for existing numeric values override.
          # The names of the label vector are the string labels, values are the numeric codes.
          # `temp_new_defined` is `stats::setNames(c(num_codes), c(string_labels))`.
          # This means `names(temp_new_defined)` are the string labels, and `temp_new_defined` itself is the numeric values.
          # The `c()` function in R for named vectors: later elements with the same *name* overwrite earlier ones.
          # But here, we want to overwrite based on *value*.
          # Correct merge logic for `labels` arg of `haven::labelled`
          # `existing_map = c(a=1, b=2)`
          # `new_map      = c(b=3, c=4)` (note: value for b changed)
          # Result should be `c(a=1, b=3, c=4)`
          r_code_lines = c(
            r_code_lines,
            "temp_existing = if (!is.null(stata2r_env$label_defs$`", lblname, "`)) stata2r_env$label_defs$`", lblname, "` else stats::setNames(numeric(0), character(0))",
            "temp_new_defined = ", label_map_r_code_str,
            # Remove from temp_existing any elements whose numeric value is present in temp_new_defined
            "values_in_new = as.numeric(temp_new_defined)", # these are the numeric values
            # To correctly remove, we need to compare values, not names.
            "temp_existing_filtered = temp_existing[! (as.numeric(temp_existing) %in% values_in_new) ]",
            # Combine, ensuring new labels for overlapping values take precedence (by order in c())
            "stata2r_env$label_defs$`", lblname, "` = c(temp_existing_filtered, temp_new_defined)"
          )
      }
  } else if (option_type == "add") { # Stata 'add' errors if any value already exists.
      # For R translation, we can mimic 'modify' as Stata itself would have errored.
      # Or, for stricter emulation, this should check and potentially error.
      # For now, treat 'add' like 'modify' for simplicity after Stata's own validation.
       r_code_lines = c(
            r_code_lines,
            "temp_existing = if (!is.null(stata2r_env$label_defs$`", lblname, "`)) stata2r_env$label_defs$`", lblname, "` else stats::setNames(numeric(0), character(0))",
            "temp_new_defined = ", label_map_r_code_str,
            "stata2r_env$label_defs$`", lblname, "` = c(temp_existing[! (as.numeric(temp_existing) %in% as.numeric(temp_new_defined)) ], temp_new_defined)"
          )
  }


  return(paste(r_code_lines, collapse="\n"))
}


t_label_values = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_label_values")
  # label values varlist lblname
  # Or: label values varlist . (to remove labels)
  values_match = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*values\\s+(.*?)\\s+([a-zA-Z_][a-zA-Z0-9_]*|\\.)$")
  if (is.na(values_match[1,1])) {
      return(paste0("# Failed to parse label values command: ", rest_of_cmd))
  }
  varlist_str = stringi::stri_trim_both(values_match[1,2])
  lblname_or_dot = values_match[1,3]

  vars_to_label = stringi::stri_split_regex(varlist_str, "\\s+")[[1]]
  vars_to_label = vars_to_label[vars_to_label != ""]

  r_code_lines = c()

  if (lblname_or_dot == ".") { # Remove labels
    for (varname in vars_to_label) {
        r_code_lines = c(r_code_lines, paste0("  data[['", varname, "']] = haven::zap_labels(data[['", varname, "']])"))
    }
  } else { # Apply labels from lblname
    lblname = lblname_or_dot
    r_code_lines = c(r_code_lines, paste0("if (!exists('stata2r_env$label_defs')) stata2r_env$label_defs = list()"))
    r_code_lines = c(r_code_lines, paste0("label_map_to_apply = stata2r_env$label_defs$`", lblname, "`"))

    for (varname in vars_to_label) {
      # Use a temporary variable for the new labelled object to avoid long line
      temp_labelled_var = paste0("stata_tmp_labelled_L", cmd_obj$line, "_", varname)

      r_code_lines = c(r_code_lines, paste0("if (!is.null(label_map_to_apply)) {"))
      r_code_lines = c(r_code_lines, paste0("  existing_var_label = attr(data[['",varname,"']], 'label')"))
      r_code_lines = c(r_code_lines, paste0("  ", temp_labelled_var, " = haven::labelled(data[['", varname, "']], labels = label_map_to_apply, label = existing_var_label)"))
      r_code_lines = c(r_code_lines, paste0("  data[['", varname, "']] = ", temp_labelled_var))
      r_code_lines = c(r_code_lines, paste0("  rm(", temp_labelled_var, ")"))
      r_code_lines = c(r_code_lines, "} else {" ) # Corrected: Removed extra ')'
      r_code_lines = c(r_code_lines, paste0("  warning(paste0('Label definition `", lblname,"` not found for `label values` command on line ", cmd_obj$line, ". Labels removed from ', '",varname,"',' if any.'))"))
      r_code_lines = c(r_code_lines, paste0("  data[['", varname, "']] = haven::zap_labels(data[['", varname, "']])"))
      r_code_lines = c(r_code_lines, "}" )
    }
  }
  return(paste(r_code_lines, collapse="\n"))
}

t_label_variable = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_label_variable")
  # label variable varname "label"
  variable_match = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*variable\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s+(?:\"([^\"]*)\"|'([^']*)')$")
  if (is.na(variable_match[1,1])) {
      return(paste0("# Failed to parse label variable command: ", rest_of_cmd))
  }
  varname = variable_match[1,2]
  label_str_double_quoted = variable_match[1,3]
  label_str_single_quoted = variable_match[1,4]

  label_str = if (!is.na(label_str_double_quoted)) label_str_double_quoted else label_str_single_quoted
  # Escape double quotes within the label string for R string literal
  label_str_escaped = stringi::stri_replace_all_fixed(label_str, '"', '\\"')


  r_code_lines = c(
      paste0("attr(data$`", varname, "`, \"label\") = \"", label_str_escaped, "\"")
  )
  return(paste(r_code_lines, collapse="\n"))
}

