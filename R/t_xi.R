# FILE: R/t_xi.R

# 1. Parsing Phase
s2r_p_xi = function(rest_of_cmd) {
  restore.point("s2r_p_xi")
  match = stringi::stri_match_first_regex(stringi::stri_trim_both(rest_of_cmd), "^i\\.([a-zA-Z_][a-zA-Z0-9_]*)(?:\\s*\\*\\s*i\\.([a-zA-Z_][a-zA-Z0-9_]*))?(?:\\s+(.*))?$")
  list(var1 = if(!is.na(match[1,1])) match[1,2] else NA_character_,
       var2 = if(!is.na(match[1,1])) match[1,3] else NA_character_)
}

# 2. Code Generation Phase
t_xi = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_xi")
  parsed = s2r_p_xi(rest_of_cmd)
  if (is.na(parsed$var1)) return(paste0("# xi command: Unsupported syntax for: ", rest_of_cmd))

  args = c("data = data", paste0("var1 = ", quote_for_r_literal(parsed$var1)))
  if (!is.na(parsed$var2)) args = c(args, paste0("var2 = ", quote_for_r_literal(parsed$var2)))

  return(paste0("data = scmd_xi(", paste(args, collapse = ", "), ")"))
}
