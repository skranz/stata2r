  # r_obj will be a single row tibble
  # at least with the field r_code
do_cmd_to_r = function(line, cmd_df) {
  cmd_obj = cmd_df[line,]

  # ignore do commands that are flagged not to
  # be translated (because they don't manipulate the data set)
  if (!cmd_obj$do_translate) return(NULL)

  r_code = NA_character_
  # TO DO: implement the translation
  # ideally write separate function for different
  # commands
  # translation functions should ideally all start
  # with do2r_ or some other common prefix
  # You may also implement other tool functions
  # that mimic stata functions. Find a sensible common prefix.


  # result object can look like this
  r_obj = data.frame(line=line, r_code = NA_character_, do_code = cmd_obj$do_code)

}
