# FILE: R/t_areg.R

t_areg = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_areg")
  t_estimation_cmd(rest_of_cmd, cmd_obj, cmd_df, line_num, context, estimator = "areg")
}
