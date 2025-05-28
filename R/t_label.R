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
  # label define lblname value "label" ... [, add|modify|replace]
  # Parts: "define ", lblname, rules, options
  define_match = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*define\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*(.*?)(?:,\\s*(.*))?$")
  if (is.na(define_match[1,1])) {
      return(paste0("# Failed to parse label define command: ", rest_of_cmd))
  }
  lblname = define_match[1,2]
  rules_str = define_match[1,3]
  options_str = define_match[1,4]

  # Parse rules: value "label" value "label" ...
  # Find all pairs of (non-space value) and (quoted label)
  rule_matches = stringi::stri_match_all_regex(rules_str, "(\\S+)\\s+\"([^\"]*)\"")[[1]]
  if (NROW(rule_matches) == 0) {
      return(paste0("# Failed to parse label define rules: ", rules_str))
  }

  values = rule_matches[,2] # Values as strings (e.g., "1", ".", ".a")
  labels = rule_matches[,3] # Label strings (e.g., "alpha", "Missing")

  # Manually construct the R named vector string for haven::labelled format: c("value" = "label_string")
  label_pairs_str = paste0('"', values, '" = "', labels, '"')
  new_labels_r_format_str = paste0("c(", paste(label_pairs_str, collapse = ", "), ")")

  # Handle options: replace, add, modify
  is_replace = !is.na(options_str) && stringi::stri_detect_fixed(options_str, "replace")
  # Stata's 'add'/'modify' behavior is effectively handled by vector concatenation with overwrite for duplicate names.

  r_code_lines = c()
  r_code_lines = c(r_code_lines, "if (!exists('stata2r_env$label_defs')) stata2r_env$label_defs = list()")
  
  if (is_replace) {
      r_code_lines = c(r_code_lines, paste0("stata2r_env$label_defs$`", lblname, "` = ", new_labels_r_format_str))
  } else {
      # If not replacing, merge existing labels with new ones. New labels overwrite old ones.
      r_code_lines = c(r_code_lines, paste0("current_labels = stata2r_env$label_defs$`", lblname, "`"))
      r_code_lines = c(r_code_lines, paste0("new_labels = ", new_labels_r_format_str))
      # Combine, new_labels (on right) will overwrite current_labels if names (values) conflict
      r_code_lines = c(r_code_lines, paste0("stata2r_env$label_defs$`", lblname, "` = c(current_labels[!names(current_labels) %in% names(new_labels)], new_labels)"))
  }

  return(paste(r_code_lines, collapse="\n"))
}


t_label_values = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  # label values varlist lblname
  values_match = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*values\\s+(.*?)\\s+([a-zA-Z_][a-zA-Z0-9_]*)$")
  if (is.na(values_match[1,1])) {
      return(paste0("# Failed to parse label values command: ", rest_of_cmd))
  }
  varlist_str = stringi::stri_trim_both(values_match[1,2])
  lblname = values_match[1,3]

  vars_to_label = stringi::stri_split_regex(varlist_str, "\\s+")[[1]]
  vars_to_label = vars_to_label[vars_to_label != ""]

  r_code_lines = c()
  r_code_lines = c(r_code_lines, paste0("if (!exists('stata2r_env$label_defs')) stata2r_env$label_defs = list()"))
  r_code_lines = c(r_code_lines, paste0("if (!is.null(stata2r_env$label_defs$`", lblname, "`)) {"))
  r_code_lines = c(r_code_lines, paste0("  label_map = stata2r_env$label_defs$`", lblname, "`"))
  for (varname in vars_to_label) {
      # Use dplyr::mutate(across()) to apply labels
      r_code_lines = c(r_code_lines, paste0("  data = dplyr::mutate(data, `", varname, "` = haven::labelled(`", varname, "`, labels = label_map))"))
  }
  r_code_lines = c(r_code_lines, "}")
  return(paste(r_code_lines, collapse="\n"))
}

t_label_variable = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  # label variable varname "label"
  variable_match = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*variable\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s+\"([^\"]*)\"$")
  if (is.na(variable_match[1,1])) {
      return(paste0("# Failed to parse label variable command: ", rest_of_cmd))
  }
  varname = variable_match[1,2]
  label_str = variable_match[1,3]

  r_code_lines = c(
      paste0("attr(data$`", varname, "`, \"label\") = \"", label_str, "\"")
  )
  return(paste(r_code_lines, collapse="\n"))
}

