# This R file shall not be changed by the AI coding agent

# do_code is a text file with one line per stata command line
# there are no comments
do_to_r = function(do_code, return_df = FALSE) {
  restore.point("do_to_r")
  do_code = stringi::stri_split_fixed(do_code, "\n")

  # transforms do_code to a dataframe with
  # one row for each code line possible
  # perform some preparsing
  # the field do_code can contain the original code
  cmd_df = do_parse(do_code)

  # will add field "do_translate"
  # if FALSE the stata command does not modify the data set
  # and can be ignored
  cmd_df = mark_data_manip_cmd(cmd_df)

  r_df = bind_rows(lapply(seq_len(NROW(cmd_df)), function(i)  {
    cmd_obj = cmd_df[i,]
    # r_obj will be a single row tibble
    # at least with the field r_code
    r_obj = do_cmd_to_r(cmd_obj=cmd_obj,line=i, cmd_df=cmd_df)
    r_obj$line = i
    r_obj$do_cmd = do_code[i]
    r_obj
  }))
  if (return_df) return(r_df)


  r_code = paste0(r_df, r_df$r_code)
  return(list(r_df=r_df, r_code=r_code))
}

