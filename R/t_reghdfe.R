# FILE: R/t_reghdfe.R

t_reghdfe = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_reghdfe")
  t_estimation_cmd(rest_of_cmd, cmd_obj, cmd_df, line_num, context, estimator = "reghdfe")
}
