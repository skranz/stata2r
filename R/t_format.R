# FILE: R/t_format.R

# 1. Parsing Phase
s2r_p_format = function(rest_of_cmd) {
  list(raw = rest_of_cmd)
}

# 2. Code Generation Phase (No-op as format only affects Stata display)
t_format = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  return("")
}
