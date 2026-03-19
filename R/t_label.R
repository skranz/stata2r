# FILE: R/t_label.R

# 1. Parsing Phase
s2r_p_label = function(rest_of_cmd) {
  restore.point("s2r_p_label")
  rest = stringi::stri_trim_both(rest_of_cmd)

  if (stringi::stri_startswith_fixed(rest, "define ")) {
    match = stringi::stri_match_first_regex(rest, "^define\\s+([a-zA-Z_][a-zA-Z0-9_]*)\\s*(.*?)(?:,\\s*(add|modify|replace))?$")
    return(list(subcmd="define", lblname=match[1,2], rules=match[1,3], option=match[1,4]))
  } else if (stringi::stri_startswith_fixed(rest, "values ")) {
    match = stringi::stri_match_first_regex(rest, "^values\\s+(.*?)\\s+([a-zA-Z_][a-zA-Z0-9_]*|\\.)$")
    return(list(subcmd="values", varlist=stringi::stri_trim_both(match[1,2]), lblname=match[1,3]))
  } else if (stringi::stri_startswith_fixed(rest, "variable ") || stringi::stri_startswith_fixed(rest, "var ")) {
    sub_rest = stringi::stri_replace_first_regex(rest, "^(?:variable|var)\\s+", "")
    match = stringi::stri_match_first_regex(sub_rest, "^([a-zA-Z_][a-zA-Z0-9_]*)\\s+(?:\"([^\"]*)\"|'([^']*)')$")
    lbl = ifelse(!is.na(match[1,2]), match[1,2], match[1,3])
    return(list(subcmd="variable", varname=match[1,1], label=lbl))
  } else if (stringi::stri_startswith_fixed(rest, "data ")) {
    sub_rest = stringi::stri_replace_first_regex(rest, "^data\\s+", "")
    match = stringi::stri_match_first_regex(sub_rest, "^(?:\"([^\"]*)\"|'([^']*)')$")
    lbl = if(!is.na(match[1,1])) ifelse(!is.na(match[1,2]), match[1,2], match[1,3]) else sub_rest
    return(list(subcmd="data", label=lbl))
  }
  return(list(subcmd=NA_character_))
}

# 2. Code Generation Phase
t_label = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_label")
  parsed = s2r_p_label(rest_of_cmd)
  if (is.na(parsed$subcmd)) return(paste0("# Failed to parse label: ", rest_of_cmd))

  args = c("data = data", paste0("subcmd = ", quote_for_r_literal(parsed$subcmd)))

  if (parsed$subcmd == "define") {
    rule_matches = stringi::stri_match_all_regex(parsed$rules, "(-?\\d*\\.?\\d+e?-?\\d*|-?\\.\\w?|\\S+)\\s+(?:\"([^\"]*)\"|'([^']*)')")[[1]]
    vals = rule_matches[,2]
    lbls = ifelse(!is.na(rule_matches[,3]), rule_matches[,3], rule_matches[,4])

    num_vals = sapply(vals, function(v) {
      if (v == "." || dplyr::coalesce(stringi::stri_detect_regex(v, "^\\.[a-zA-Z]$"), FALSE)) NA_real_ else as.numeric(v)
    })

    map_str = "stats::setNames(numeric(0), character(0))"
    if (length(lbls) > 0) {
      map_str = paste0("stats::setNames(c(", paste(ifelse(is.na(num_vals), "NA_real_", num_vals), collapse=", "), "), c('", paste(lbls, collapse="','"), "'))")
    }

    args = c(args, paste0("lblname = ", quote_for_r_literal(parsed$lblname)), paste0("rules_map = ", map_str), paste0("option = ", quote_for_r_literal(parsed$option)))

  } else if (parsed$subcmd == "values") {
    args = c(args, paste0("varlist = ", quote_for_r_literal(parsed$varlist)), paste0("lblname = ", quote_for_r_literal(parsed$lblname)))
  } else if (parsed$subcmd == "variable") {
    args = c(args, paste0("varname = ", quote_for_r_literal(parsed$varname)), paste0("label_str = ", quote_for_r_literal(parsed$label)))
  } else if (parsed$subcmd == "data") {
    args = c(args, paste0("label_str = ", quote_for_r_literal(parsed$label)))
  }

  return(paste0("data = scmd_label(", paste(args, collapse = ", "), ")"))
}

# 3. Runtime Execution Phase
scmd_label = function(data, subcmd, lblname=NA, rules_map=NULL, option=NA, varlist=NA, varname=NA, label_str=NA) {
  restore.point("scmd_label")
  if (!exists("label_defs", envir = stata2r_env)) stata2r_env$label_defs = list()

  if (subcmd == "define") {
    existing = if (!is.null(stata2r_env$label_defs[[lblname]])) stata2r_env$label_defs[[lblname]] else stats::setNames(numeric(0), character(0))
    if (is.na(option) || option == "replace") {
      stata2r_env$label_defs[[lblname]] = rules_map
    } else {
      existing_filtered = existing[!(as.numeric(existing) %in% as.numeric(rules_map))]
      stata2r_env$label_defs[[lblname]] = c(existing_filtered, rules_map)
    }
  } else if (subcmd == "values") {
    cols = expand_varlist(varlist, names(data))
    if (lblname == ".") {
      for (c in cols) data[[c]] = haven::zap_labels(data[[c]])
    } else {
      map = stata2r_env$label_defs[[lblname]]
      for (c in cols) {
        if (!is.null(map)) {
          ex_lbl = attr(data[[c]], "label")

          # Fix: haven requires NULL instead of NA_character_ if no label exists
          lbl_arg = if(is.null(ex_lbl) || length(ex_lbl) == 0) NULL else as.character(ex_lbl)[1]
          if (!is.null(lbl_arg) && is.na(lbl_arg)) lbl_arg = NULL

          # Strip existing S3 classes cleanly to avoid haven conflict errors
          base_val = unclass(data[[c]])
          attr(base_val, "labels") = NULL
          attr(base_val, "label") = NULL

          data[[c]] = haven::labelled(base_val, labels = map, label = lbl_arg)
        } else {
          data[[c]] = haven::zap_labels(data[[c]])
        }
      }
    }
  } else if (subcmd == "variable") {
    cols = expand_varlist(varname, names(data))
    for (c in cols) attr(data[[c]], "label") = label_str
  } else if (subcmd == "data") {
    attr(data, "label") = label_str
  }
  return(data)
}
