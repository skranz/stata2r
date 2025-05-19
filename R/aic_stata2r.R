example = function() {
  library(aicoder)
  main_dir = "~/aicoder"
  files = paste0(main_dir, "/stata2r/R/",  c("aic_stata2r.R", "aic_eval_ex.R","aic_stata_ex.R"))
  for (file in files)  source(file)
  aic = aic_new_stata2r(main_dir)
  aic = aic_test_stata2r(aic)
  aic = aic_make_prompt_stata2r(aic)

  # Manuall run prompt
  if (FALSE) {
    aic$response_file
    aic = aic_changes_stata2r(aic)
  }
  aic_view_prompt(aic)

  do_file = "C:/libraries/aicoder/stata2r/inst/cases/custom_1/do1.do"
}


aic_new_stata2r = function(main_dir) {
  main_dir = normalizePath(main_dir)
  repo_dir=file.path(main_dir, "stata2r")
  #do_file = "C:/libraries/aicoder/stata2r/inst/cases/custom_1/do1.do"
  aic = aic_new(
    log_base_dir = file.path(main_dir, "aic_log/stata2r"),
    repo_dir=repo_dir,
    is_pkg=TRUE,
    pat_file = file.path(main_dir, "pat.txt"),
    prompt_config_file = file.path(repo_dir,"f2p_stata2r.toml"),
    do_files = paste0(repo_dir,"inst/cases/", c("custom_1/do1.do")),
    response_file = file.path(repo_dir, "aicoder_work/ai_resp.txt"),
    temp_dir = file.path(main_dir, "temp"),
    mod_protected_files = c("R/aic_stata2r.R","R/main.R","R/aic_stata_ex.R"),
    mod_fixed_dirs = c("r"="R"),
    mod_just_ext = c("r"),
    show_test = FALSE,
    show_failed_test = FALSE
  )

  aic

}

aic_make_prompt_stata2r = function(aic) {
  restore.point("aic_make_prompt_stata2r")
  num_test_failed = aic_num_test_failed(aic)
  num_test = aic_num_test(aic)
  test_str = aic$test_report
  cfg=aic$cfg
  if (!is.null(test_str)) {
    test_str = paste0("\n#PERFORMED TESTS ON CURRENT VERSION:\n",
      "In total ", num_test_failed, " of ", num_test, " tests failed.\n\n",
      test_str)
  } else {
    test_str = ""
  }
  cfg$tests = test_str
  task = aic$cfg$task
  if (num_test_failed > 0) {
    task = paste0(task,
"\n# Main task given failed tests

As shown further above, the current code base has failed some tests.
Thus your main task now is to correct the current code base.
Other modifications can be performed once all tests pass correctly.
")
  }
  cfg$task = task
  aic = aic_make_prompt(aic, cfg)
  aic
}

aic_changes_stata2r = function(aic, resp_text = NULL) {
  restore.point("aic_stata2r_update")
  library(aicoder)
  aic_parse_and_commit_changes(aic, resp_text)
}

aic_test_stata2r = function(aic) {
  aic = aic_clear_tests(aic)
  aic = aic_test_source_r_files(aic)
  if (aic_num_test_failed(aic)>0) {
    cat("\nFailed sourcing R files in stata2r/R")
    aic = aic_tests_finish(aic)
    return(aic)
  }
  cat("\nSourced R files in stata2r/R without error...\n")



  # Build package? Maybe not needed if everything is sourced
  library(aicoder)
  files = list.files(file.path(aic$repo_dir,"R"),glob2rx("*.R"), full.names = TRUE)

  for (file in files) source(file)

  tests_dir = file.path(aic$repo_dir,"aicoder_work", "tests")
  test_scripts = list.files(tests_dir, glob2rx("test_*.R"), full.names = TRUE, recursive = TRUE)

  test_script = test_scripts[1]
  aic = aic_test_script(aic,test_script)

  aic = aic_tests_finish(aic)
  aic
}



aic_stata2r_deploy_tests = function(test_dir) {
  test_dir = "C:/libraries/aicoder/stata2r/aicoder_work/tests"
  case_dirs = list.dirs("C:/libraries/aicoder/stata2r/inst/cases",full.names = TRUE, recursive=FALSE)

  # init base files in test dir
  case_dir = case_dirs[1]
  files = list.files(case_dir,full.names = TRUE)
  dest_dir = file.path("C:/libraries/aicoder/stata2r/aicoder_work/tests", basename(case_dir))
  file.copy(files, dest_dir)

  templ_file = file.path("C:/libraries/aicoder/stata2r/inst/test_tpl/test_tpl.R")
  tpl = read_utf8(templ_file)

  values = list()
  do_file = list.files(dest_dir, glob2rx("*.do"), full.names = TRUE)[1]
  values$do_file = do_file
  values$test_dir = dest_dir

  test_code = files2prompt:::tpl_replace_whisker(tpl, values)
  test_file = paste0(dest_dir, "/test_", basename(dest_dir), ".R")
  write_utf8(test_code, test_file)
}

