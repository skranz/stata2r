# FILE: R/t_sort.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_sort = function(rest_of_cmd) {
  restore.point("s2r_p_sort")
  list(varlist = stringi::stri_trim_both(rest_of_cmd))
}

# 2. Code Generation Phase: Emit R code
t_sort = function(rest_of_cmd, cmd_obj, cmd_df, line_num, type = "sort") {
  restore.point("t_sort")
  parsed = s2r_p_sort(rest_of_cmd)

  if (is.na(parsed$varlist) || parsed$varlist == "") {
    return("# sort/gsort command with no variables specified.")
  }

  args = c("data = data",
           paste0("varlist_str = ", quote_for_r_literal(parsed$varlist)),
           paste0("type = ", quote_for_r_literal(type)))

  r_code_str = paste0("data = scmd_sort(", paste(args, collapse = ", "), ")")

  # Maintain package internal tracking variables (updating after sort)
  if (isTRUE(stata2r_env$has_original_order_idx)) {
    r_code_str = paste0(r_code_str, " %>% \n  dplyr::mutate(stata2r_original_order_idx = dplyr::row_number())")
  }

  return(r_code_str)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_sort = function(data, varlist_str, type = "sort") {
  restore.point("scmd_sort")

  tokens = stringi::stri_split_regex(trimws(varlist_str), "\\s+")[[1]]
  tokens = tokens[tokens != ""]

  if (length(tokens) == 0) return(data)

  sort_exprs = character(0)

  # Resolve +/- and wildcards correctly
  for (tok in tokens) {
    desc = FALSE
    if (type == "gsort") {
      if (startsWith(tok, "-")) {
        desc = TRUE
        tok = substring(tok, 2)
      } else if (startsWith(tok, "+")) {
        tok = substring(tok, 2)
      }
    }

    expanded = expand_varlist(tok, names(data))
    for (v in expanded) {
      if (desc) {
        sort_exprs = c(sort_exprs, paste0("dplyr::desc(`", v, "`)"))
      } else {
        sort_exprs = c(sort_exprs, paste0("`", v, "`"))
      }
    }
  }

  if (length(sort_exprs) > 0) {
    # Dynamically evaluate the correctly expanded and scoped expressions
    cmd = paste0("dplyr::arrange(data, ", paste(sort_exprs, collapse = ", "), ")")
    data = eval(parse(text = cmd))
  }

  return(data)
}
