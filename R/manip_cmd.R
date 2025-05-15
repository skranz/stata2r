mark_data_manip_cmd = function(cmd_df) {
  # determine commands that actually can transform the Stata data
  # set or generate scalars that will be used in later
  # commands that change the data set
  # only those commands shall be translated to R

  # currently just set all TRUE, but change that code
  cmd_df$do_translate = rep(TRUE, NROW(cmd_df))
  cmd_df
}
