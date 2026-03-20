# FILE: R/t_probit.R

t_probit = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_probit")
  t_estimation_cmd(rest_of_cmd, cmd_obj, cmd_df, line_num, context, estimator = "probit")
}
