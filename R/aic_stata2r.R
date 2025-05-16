example = function() {
  library(aicoder)
  main_dir = "C:/libraries/aicoder"
  source("C:/libraries/aicoder/stata2r/R/aic_stata2r.R")
  aic = aic_new_stata2r(main_dir)
  aic = aic_changes_stata2r(aic)
  aic = aic_test_stata2r(aic)
  aic = aic_make_prompt_stata2r(aic)
  aic_view_prompt(aic)

  do_file = "C:/libraries/aicoder/stata2r/inst/cases/custom_1/do1.do"
}

aic_new_stata2r = function(main_dir) {
  repo_dir=file.path(main_dir, "stata2r")
  do_file = "C:/libraries/aicoder/stata2r/inst/cases/custom_1/do1.do"
  aic = aic_new(
    repo_dir=repo_dir,
    is_pkg=TRUE,
    pat_file = file.path(main_dir, "pat.txt"),
    prompt_config_file = file.path(repo_dir,"f2p_stata2r.toml"),
    main_dir = main_dir,
    do_files = paste0(repo_dir,"inst/cases/", c("custom_1/do1.do")),
    response_file = file.path(main_dir, "ai_resp.txt"),
    temp_dir = file.path(main_dir, "temp"),
    mod_protected_files = c("R/aic_stata2r.R"),
    mod_fixed_dirs = c("r"="R"),
    mod_just_ext = c("r")
  )

  aic

}

aic_make_prompt_stata2r = function(aic) {
  restore.point("aic_make_prompt_stata2r")
  num_test_failed = aic_num_test_failed(aic)
  num_test = aic_num_test(aic)
  test_str = aic_all_test_string(aic)
  cfg=aic$cfg
  if (!is_empty(test_str)) {
    test_str = paste0("\n#PERFORMED TESTS ON CURRENT VERSION:\n",
      "In total ", num_test_failed, " of ", num_test, " tests failed.\n\n",
      test_str)
  }
  cfg$test = test_str
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
  aic = aic_test_build_pkg(aic)
  aic
}

aic_stata2r_run_example = function(do_file) {
  library(stata2r)
  do_dir = dirname(do_file)
  do_text = readLines(do_file)
  r_code = do_to_r(do_code)
}
