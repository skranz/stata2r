example = function() {
  main_dir = "C:/libraries/aicoder"
  aic = aic_new_stata2r(main_dir)
  do_file = "C:/libraries/aicoder/stata2r/inst/cases/custom_1/do1.do"
}

aic_new_stata2r = function(main_dir) {
  repo_dir=file.path(main_dir, "stata2r")
  aic = aic_new(
    repo_dir=repo_dir,
    is_pkg=TRUE,
    pat_file = file.path(main_dir, "pat.txt"),
    prompt_config_file = file.path(repo_dir,"f2p_stata2r.toml"),
    main_dir = main_dir,
    do_files = paste0(repo_dir,"inst/cases/", c("custom_1/do1.do"))
  )

  parent_dir = "C:/libraries/aicoder"
  response_file = "ai_resp.txt"
  aic_stata2r_update(parent_dir, response_file)
  do_file = "C:/libraries/aicoder/stata2r/inst/cases/custom_1/do1.do"


}

telo_stata2r = function() {
  telo = telo_new()
  telo = telo %>% telo_add_step_build_pkg(telo, "")
}

aic_stata2r_update = function(parent_dir, response_file, pat_file="pat.txt", text = NULL) {
  restore.point("aic_stata2r_update")
  library(aicoder)
  if (is.null(text)) {
    text = readLines(file.path(parent_dir, response_file))
  }
  pat_file = file.path(parent_dir, pat_file)
  repo_dir = file.path(parent_dir,"stata2r")

  changes = aic_parse_response(text)
  protected_files = c("aic_stata2r.R", "main.R")
  just_ext = "r"
  fixed_dirs = c("r"="R")
  aic_apply_changes_and_github(changes, repo_dir=repo_dir, protected_files = protected_files, just_ext=just_ext, fixed_dirs=fixed_dirs, pat_file=pat_file)
}

aic_stata2r_run_example = function(do_file) {
  library(stata2r)
  do_dir = dirname(do_file)
  do_text = readLines(do_file)
  r_code = do_to_r(do_code)
}
