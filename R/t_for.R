# FILE: R/t_for.R

# 1. Parsing Phase
s2r_p_for = function(rest_of_cmd) {
  restore.point("s2r_p_for")
  # match[1,1] full match
  # match[1,2] list type (num, any, val, var, varlist, numlist, etc.)
  # match[1,3] list def (1/12 or _Ii*)
  # match[1,4] command body (gen lnlitmX = log(eapmX ))
  match = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*([a-zA-Z]+)\\s+(.*?)\\s*:\\s*(.*)$")
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

  is_varlist = startsWith(tolower(parsed$list_type), "var")

  if (!is_varlist) {
    if (startsWith(tolower(parsed$list_type), "num")) {
      elements = expand_stata_numlist(parsed$list_def)
    } else {
      # "any" or "val" are just space-separated tokens
      elements = stringi::stri_split_regex(parsed$list_def, "\\s+")[[1]]
      elements = elements[elements != ""]
    }

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

  } else {

    # Varlist needs runtime expansion because columns are only known at runtime
    cmd_sub = gsub("X", "_S2R_X_", parsed$cmd_body, fixed = TRUE)
    cmd_sub = gsub("@", "_S2R_AT_", cmd_sub, fixed = TRUE)

    sub_parsed = parse_stata_command_line(cmd_sub)

    if (is.na(sub_parsed$stata_cmd)) {
      return(paste0("# Failed to parse substituted command: ", cmd_sub))
    }

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

    r_obj = do_cmd_to_r(sub_cmd_obj, line_num, cmd_df)

    # Use quote_for_r_literal to safely package the generated R code block as a string argument
    r_code_template_escaped = quote_for_r_literal(r_obj$r_code)

    args = c(
      "data = data",
      paste0("varlist_str = ", quote_for_r_literal(parsed$list_def)),
      paste0("r_code_template = ", r_code_template_escaped)
    )

    return(paste0("data = scmd_for(", paste(args, collapse = ", "), ")"))
  }
}

# 3. Runtime Execution Phase
scmd_for = function(data, varlist_str, r_code_template) {
  restore.point("scmd_for")

  # Find the actual columns that match the varlist
  vars = expand_varlist(varlist_str, names(data))

  # Create an isolated environment to execute the inner command.
  # This prevents local loop variables from leaking, while allowing
  # modifications to the `data` object to persist.
  eval_env = new.env(parent = parent.frame())
  eval_env$data = data

  for (v in vars) {
    # Instantiate the template for the current variable
    code = gsub("_S2R_X_", v, r_code_template, fixed = TRUE)
    code = gsub("_S2R_AT_", v, code, fixed = TRUE)

    # Execute safely. If the code modifies data (e.g. data = scmd_generate(...)),
    # eval_env$data is updated.
    tryCatch({
      eval(parse(text = code), envir = eval_env)
    }, error = function(e) {
      stop(paste0("Error in scmd_for evaluating variable '", v, "':\n", e$message))
    })
  }

  return(eval_env$data)
}
