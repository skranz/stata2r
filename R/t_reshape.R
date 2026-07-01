# FILE: R/t_reshape.R

# 1. Parsing Phase
# 1. Parsing Phase
s2r_p_reshape = function(rest_of_cmd) {
  restore.point("s2r_p_reshape")
  match = stringi::stri_match_first_regex(stringi::stri_trim_both(rest_of_cmd), "^\\s*(wide|long)\\s+(.*?)(?:,\\s*(.*))?$")
  if (is.na(match[1,1])) return(list(type = NA_character_))

  type = match[1,2]
  stubnames = stringi::stri_split_regex(stringi::stri_trim_both(match[1,3]), "\\s+")[[1]]
  options_str = stringi::stri_trim_both(match[1,4])

  i_vars = NA_character_
  j_var = NA_character_
  j_is_string = FALSE

  if (!is.na(options_str)) {
    i_match = stringi::stri_match_first_regex(options_str, "\\bi\\s*\\(([^)]+)\\)")
    if (!is.na(i_match[1,1])) i_vars = stringi::stri_trim_both(i_match[1,2])

    j_match = stringi::stri_match_first_regex(options_str, "\\bj\\s*\\(([^)]+)\\)")
    if (!is.na(j_match[1,1])) {
      j_part = stringi::stri_trim_both(j_match[1,2])
      j_var_match = stringi::stri_match_first_regex(j_part, "^\\s*([a-zA-Z_][a-zA-Z0-9_]*)")
      if (!is.na(j_var_match[1,1])) {
        j_var = j_var_match[1,2]
        j_is_string = grepl("\\bstring\\b", j_part)
      }
    }
  }

  list(type = type, stubs = stubnames[stubnames != ""], i_vars = i_vars, j_var = j_var, j_is_string = j_is_string)
}

# 2. Code Generation Phase
# 2. Code Generation Phase
t_reshape = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_reshape")
  parsed = s2r_p_reshape(rest_of_cmd)
  if (is.na(parsed$type)) return(paste0("# Failed to parse reshape: ", rest_of_cmd))

  args = c("data = data", paste0("type = ", quote_for_r_literal(parsed$type)),
           paste0("stubs_str = ", quote_for_r_literal(paste(parsed$stubs, collapse=" "))),
           paste0("i_vars = ", quote_for_r_literal(parsed$i_vars)),
           paste0("j_var = ", quote_for_r_literal(parsed$j_var)),
           paste0("j_is_string = ", parsed$j_is_string))

  r_code = paste0("data = scmd_reshape(", paste(args, collapse = ", "), ")")
  r_code = paste0(r_code, "\nassign(\"has_original_order_idx\", TRUE, envir = stata2r_env)")
  return(r_code)
}

# 3. Runtime Execution Phase
# 3. Runtime Execution Phase
# 3. Runtime Execution Phase
scmd_reshape = function(data, type, stubs_str, i_vars = NA_character_, j_var = NA_character_, j_is_string = FALSE) {
  restore.point("scmd_reshape")

  if (type == "wide") {
    # In wide reshape, stubs represent existing variable names, so we expand any varlist patterns (e.g. var1-var5)
    stubs = expand_varlist(stubs_str, names(data))
    if (length(stubs) == 0) {
      stop(paste0("scmd_reshape: no variables found matching '", stubs_str, "'"))
    }

    # tidyr::pivot_wider automatically uses all columns not in names_from/values_from as ID columns.
    # This naturally includes Stata's i() variables and preserves other constant variables just like Stata does.
    data = tidyr::pivot_wider(data, names_from = dplyr::all_of(j_var), values_from = dplyr::all_of(stubs), names_sep = "")
  } else if (type == "long") {
    # In long reshape, stubs act as prefixes for currently wide data columns
    stubs = stringi::stri_split_regex(stubs_str, "\\s+")[[1]]
    stubs = stubs[stubs != ""]

    if (j_is_string) {
      cols_regex = paste0("^(", paste(stubs, collapse = "|"), ").+$")
      names_pattern = paste0("^(", paste(stubs, collapse = "|"), ")(.+)$")
    } else {
      cols_regex = paste0("^(", paste(stubs, collapse = "|"), ")[0-9]+$")
      names_pattern = paste0("^(", paste(stubs, collapse = "|"), ")([0-9]+)$")
    }
    names_to = c(".value", j_var)

    # tidyr::pivot_longer automatically keeps all non-matched columns (which includes Stata's i() variables)
    # as the identifier columns and duplicates them appropriately.
    data = tidyr::pivot_longer(data, cols = dplyr::matches(cols_regex), names_to = names_to, names_pattern = names_pattern)

    if (!j_is_string) data[[j_var]] = as.numeric(data[[j_var]])
  }

  data = sfun_normalize_string_nas(data)
  data$stata2r_original_order_idx = seq_len(nrow(data))
  return(data)
}
