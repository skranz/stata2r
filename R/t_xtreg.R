# FILE: R/t_xtreg.R

t_xtreg = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_xtreg")
  t_estimation_cmd(rest_of_cmd, cmd_obj, cmd_df, line_num, context, estimator = "xtreg")
}
