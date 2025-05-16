library(stata2r)
setwd("C:/libraries/aicoder/stata2r/aicoder_work/tests/do1")

# do code that will be translated
do_code = readLines("C:/libraries/aicoder/stata2r/aicoder_work/tests/do1/do1.do", warn=FALSE)
cat(do_code, sep="\n")


# transforms do_code to a dataframe with
# one row for each code line possible
# perform some preparsing
# the field do_code should contain the original code
cmd_df = do_parse(do_code)

# will add field "do_translate"
# if FALSE the stata command does not modify the data set
# and can be ignored
cmd_df = mark_data_manip_cmd(cmd_df)
str(cmd_df)

r_li = vector("list", NROW(cmd_df))

for (i in seq_len(r_li)) {
  cat("\n", i,"of", length(r_li), "translate", cmd_df$do_code[[i]],"...\n")
  cmd_obj = cmd_df[i,]
  r_obj = do_cmd_to_r(cmd_obj=cmd_obj,line=i, cmd_df=cmd_df)
  print(str(r_obj))
  cat("\nR code:\n",r_obj$r_code,"\n")
  r_li[[i]] = r_obj
}
r_df = bind_rows(r_li)


r_code = paste0(r_df, r_df$r_code)
writeLines(r_code, "test_rcode.R")
