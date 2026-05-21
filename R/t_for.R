# FILE: R/t_for.R

# 1. Parsing Phase
s2r_p_for = function(rest_of_cmd) {
  restore.point("s2r_p_for")
  # match[1,1] full match
  # match[1,2] list type (num, any, val, var, etc.)
  # match[1,3] list def (1/12)
  # match[1,4] command body (gen lnlitmX = log(eapmX ))
  match = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*(num|any|val|var)\\s+(.*?)\\s*:\\s*(.*)$")
  if (is.na(match[1,1])) {
    return(list(list_type = NA_character_, list_def = NA_character_, cmd_body = NA_character_))
  }

  list(list_type = stringi::stri_trim_both(match[1,2]),
       list_def = stringi::stri_trim_both(match[1,3]),
       cmd_body = stringi::stri_trim_both(match[1,4]))
}

# 2. Code Generation Phase
t_for = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_for")
  parsed = s2r_p_for(rest_of_cmd)
  if (is.na(parsed$list_type)) {
    return(paste0("# Failed to parse for loop: ", rest_of_cmd))
  }

  elements = expand_stata_numlist(parsed$list_def)
  if (length(elements) == 0) {
    return(paste0("# for loop list is empty: ", parsed$list_def))
  }

  r_codes = character(0)

  for (el in elements) {
    # Replace X and @
    cmd_sub = gsub("X", el, parsed$cmd_body, fixed = TRUE)
    cmd_sub = gsub("@", el, cmd_sub, fixed = TRUE)

    # Parse the substituted command
    sub_parsed = parse_stata_command_line(cmd_sub)

    if (is.na(sub_parsed$stata_cmd)) {
      r_codes = c(r_codes, paste0("# Failed to parse substituted command: ", cmd_sub))
      next
    }

    # Create a dummy cmd_obj to pass to do_cmd_to_r
    sub_cmd_obj = cmd_obj
    sub_cmd_obj$stata_cmd = sub_parsed$stata_cmd
    sub_cmd_obj$stata_cmd_original = sub_parsed$stata_cmd_original
    sub_cmd_obj$rest_of_cmd = sub_parsed$rest_of_cmd
    sub_cmd_obj$do_code = cmd_sub
    sub_cmd_obj$is_quietly_prefix = sub_parsed$is_quietly_prefix
    sub_cmd_obj$is_capture_prefix = sub_parsed$is_capture_prefix
    sub_cmd_obj$is_by_prefix = sub_parsed$is_by_prefix
    sub_cmd_obj$is_bysort_prefix = sub_parsed$is_bysort_prefix
    sub_cmd_obj$by_group_vars = if(length(sub_parsed$by_group_vars)>0) paste(sub_parsed$by_group_vars, collapse=",") else NA_character_
    sub_cmd_obj$by_sort_vars = if(length(sub_parsed$by_sort_vars)>0) paste(sub_parsed$by_sort_vars, collapse=",") else NA_character_
    sub_cmd_obj$is_xi_prefix = sub_parsed$is_xi_prefix
    sub_cmd_obj$do_translate = TRUE

    # Delegate to the general translation function
    r_obj = do_cmd_to_r(sub_cmd_obj, line_num, cmd_df)
    r_codes = c(r_codes, r_obj$r_code)
  }

  return(paste(r_codes, collapse = "\n"))
}
