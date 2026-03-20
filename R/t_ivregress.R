# FILE: R/t_ivregress.R

t_ivregress = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_ivregress")
  t_estimation_cmd(rest_of_cmd, cmd_obj, cmd_df, line_num, context, estimator = "ivregress")
}
