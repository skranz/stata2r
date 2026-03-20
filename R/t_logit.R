# FILE: R/t_logit.R

t_logit = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_logit")
  t_estimation_cmd(rest_of_cmd, cmd_obj, cmd_df, line_num, context, estimator = "logit")
}
