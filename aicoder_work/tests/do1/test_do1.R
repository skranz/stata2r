library(stata2r)

# do code that will be translated
do_code = readLines("do1.do", warn=FALSE)
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

for (i in seq_along(r_li)) {
  #cat("\n", i,"of", length(r_li), "translate", cmd_df$do_code[[i]],"\n")
  cat("\n# ", i,"of", length(r_li), "translate: ", cmd_df$do_code[[i]],"\n")
  cmd_obj = cmd_df[i,]
  r_obj = do_cmd_to_r(cmd_obj=cmd_obj,line=i, cmd_df=cmd_df)
  #print(str(r_obj))
  if (isTRUE(cmd_df$do_translate[i])) {
    cat("\nR code:",r_obj$r_code,"\n")
    r_li[[i]] = r_obj
  }
}
r_df = bind_rows(r_li)

env = new.env(parent=globalenv())

i = 1
log_str = NULL
for (i in NROW(r_df)) {
  r_code = r_df$r_code[[i]]
  res = aicoder::run_with_log(code_str=r_code, env=env)
  res$has_error

}
cat(r_code)
env$data
res= evaluate::evaluate(r_code,envir = env)
as.character(res)


f = function() {
  cat("hi")
  "u"+5
}

out_and_err_txt = function(out, err=NULL) {
  if (is(err,"try-error")) {
    out = c(out,as.character(err))
  }
  paste0(out, collapse="\n")
}

txt = capture.output(err<-try(f(), silent=TRUE))
cat(out_and_err_txt(txt, err))

r_code = paste0(r_df$r_code, collapse="\n")
writeLines(r_code, "r_trans.R")
