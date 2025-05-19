examples = function() {
  do_file = "C:/libraries/aicoder/stata2r/inst/cases/custom_1/do1.do"

}

aic_stata2r_eval_example = function(do_file) {
  library(stata2r)
  do_dir = dirname(do_file)
  do_text = readLines(do_file)
  r_code = do_to_r(do_code)

  library(stata2r)
  library(aicoder)

}
