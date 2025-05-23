# Translate Stata 'sort' and 'gsort' commands
# Stata: sort varlist
# Stata: gsort [+|-]varname [[+|-]varname ...]
t_sort = function(rest_of_cmd, cmd_obj, cmd_df, line_num, type = "sort") {
  restore.point("t_sort") # Added restore.point
  if (is.na(rest_of_cmd) || rest_of_cmd == "") {
    return("# sort/gsort command with no variables specified.")
  }

  varlist = stringi::stri_trim_both(rest_of_cmd)
  vars = stringi::stri_split_regex(varlist, "\\s+")[[1]]
  vars = vars[vars != ""]

  if (length(vars) == 0) {
    return("# sort/gsort command with no effectively parsed variables.")
  }

  if (type == "sort") {
    # Plain sort is ascending for all variables
    # Using dplyr::arrange for consistency with gsort and reliability
    sort_vars_r = paste(vars, collapse = ", ") # pass as bare names
    r_code_str = paste0("data = dplyr::arrange(data, ", sort_vars_r, ")")

  } else if (type == "gsort") {
    # gsort allows specifying ascending (+) or descending (-) for each variable
    # +var (ascending, default if no sign)
    # -var (descending)
    # dplyr: arrange(var1, desc(var2), ...) is more flexible here
    arrange_expressions = character(length(vars))
    for (i in seq_along(vars)) {
      var_spec = vars[i]
      if (stringi::stri_startswith_fixed(var_spec, "-")) {
        var_name = stringi::stri_sub(var_spec, 2)
        arrange_expressions[i] = paste0("dplyr::desc(", var_name, ")")
      } else if (stringi::stri_startswith_fixed(var_spec, "+")) {
        var_name = stringi::stri_sub(var_spec, 2)
        arrange_expressions[i] = var_name
      } else {
        arrange_expressions[i] = var_spec
      }
    }
    r_code_str = paste0("data = dplyr::arrange(data, ", paste(arrange_expressions, collapse = ", "), ")")
  } else {
    r_code_str = paste0("# Unknown sort type: ", type)
  }

  return(r_code_str)
}

