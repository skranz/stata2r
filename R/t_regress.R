# FILE: R/t_regress.R

t_regress = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_regress")
  t_estimation_cmd(rest_of_cmd, cmd_obj, cmd_df, line_num, context, estimator = "regress")
}

scmd_regress = function(data, dep_var, indep_vars, needed_e, r_if_cond = NA_character_) {
  restore.point("scmd_regress")
  scmd_estimation_effects(
    data = data,
    dep_var = dep_var,
    model_vars = indep_vars,
    needed_e = needed_e,
    r_if_cond = r_if_cond,
    estimator = "regress",
    formula_terms = indep_vars
  )
}
