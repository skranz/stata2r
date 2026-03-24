# FILE: /home/rstudio/aicoder/stata2r/R/t_scalar.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_scalar = function(rest_of_cmd) {
  restore.point("s2r_p_scalar")
  rest = stringi::stri_trim_both(stringi::stri_replace_first_regex(rest_of_cmd, "^define\\s+", ""))
  parts = stringi::stri_split_fixed(rest, "=", n = 2)[[1]]
  if (length(parts) == 2) {
    return(list(
      name = stringi::stri_trim_both(parts[1]),
      expr = stringi::stri_trim_both(parts[2])
    ))
  }
  return(list(name = NA_character_, expr = NA_character_))
}

# 2. Code Generation Phase: Emit R code
t_scalar = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_scalar")
  parsed = s2r_p_scalar(rest_of_cmd)
  if (is.na(parsed$name)) return(paste0("# Failed to parse scalar: ", rest_of_cmd))

  r_expr = translate_stata_expression_with_r_values(parsed$expr, line_num, cmd_df, list(is_by_group = FALSE))

  args = c("data = data", paste0("r_expr_str = ", quote_for_r_literal(r_expr)))

  return(paste0("`", parsed$name, "` = scmd_scalar(", paste(args, collapse = ", "), ")"))
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_scalar = function(data, r_expr_str) {
  restore.point("scmd_scalar")
  eval_list = s2r_setup_eval_list(data)
  res = base::eval(base::parse(text = r_expr_str), envir = eval_list, enclos = parent.frame())
  if (length(res) > 0) res[1] else NA_real_
}
