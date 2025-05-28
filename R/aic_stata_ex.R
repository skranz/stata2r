# Tools to make Stata example

example = function() {
  do_file = normalizePath("~/aicoder/stata2r/inst/cases/do1/do1.do")
  run_do_with_data_save(do_file)
  do_file = normalizePath("~/aicoder/stata2r/inst/cases/do2/do2.do")
  run_do_with_data_save(do_file)

  do_file = normalizePath("~/aicoder/stata2r/inst/cases/do3/do3.do")
  run_do_with_data_save(do_file)

}

run_do_with_data_save = function(do_file) {
  restore.point("run_do_with_data_save")
  library(dplyr)
  dir = dirname(do_file)
  id = tools::file_path_sans_ext(basename(do_file))
  do_code = readLines(do_file,warn = FALSE)
  setwd(dir)
  lines = seq_along(do_code)
  dta_dir = file.path(dir, "do_data")
  if (!dir.exists(dta_dir)) dir.create(dta_dir)
  do_data_file = paste0(dir,"/do_data/",id,"-", lines,".dta")
  mod_code = paste0(do_code, '\nsave "', do_data_file,'", replace emptyok')
  temp_file = paste0(dir, "/temp_", id, ".do")
  writeLines(mod_code, temp_file)

  run_stata_do(temp_file)

  # make data codes
  dta_files = paste0(dta_dir, "/",id,"-", lines,".dta")
  hashes = sapply(dta_files, function(dta_file) {
    data = read_dta(dta_file)
    data_quick_hash(data)
  })
  hash_df = tibble(doid=id, row=seq_along(hashes), dta_file=basename(dta_files), quick_hash=hashes) %>%
    mutate(hash_changed=!is.true(quick_hash==lag(quick_hash)))
  saveRDS(hash_df, file.path(dir,"data_hashes.Rds"))
}

data_quick_hash = function(df) {
  str = paste0(NROW(df),"|",NCOL(df),"|",
    paste0(names(df), collapse=";")
  )
  row = ceiling(NROW(df)/2)
  if (row>0) {
    el = as.list(df[row,])
    el_str = paste0(sapply(el,function(f) substr(as.character(f)[1],1,5)), collapse=";")
    str = paste0(str,"|",el_str )
  }
  str
}



run_stata_do = function(do.file, stata_bin=get_stata_bin(), set.dir=TRUE, nostop=TRUE, timeout=60*5, verbose=TRUE, use.timeout = !is_windows, is_windows =  .Platform$OS.type == "windows") {
  restore.point("run_stata_do")
  if (set.dir) {
    do.dir = dirname(do.file)
    old.dir = getwd()
    setwd(do.dir)
    file = basename(do.file)
  } else {
    file = do.file
  }


  if (verbose) {
    cat("\n\nRun ", do.file)
  }

  if (is_windows & nostop) {
    cmd = paste0(stata_bin,' /e do "',file,'"', if(nostop) ', nostop')
    #
    # Note: On Windows in BATCH mode no shell commands can be executed.
    # This means rcall cannot be used.
  } else if (is_windows & !nostop) {
    #stop("Need to check windows without nostop")
    cmd = paste0(stata_bin,' /e "',file,'"')
  } else if (!nostop) {
    # Without nostop we must write the code differently (at least on Linux)
    # without explicitly calling the do command
    cmd = paste0(stata_bin,' -b "',file,'"')
    if (!is_empty(timeout) & use.timeout) {
      cmd = paste0("timeout ", timeout," ", cmd)
    }
  } else if (!is_empty(timeout) & use.timeout) {
    cmd = paste0(stata_bin,' -b \'do "',file,'"', if(nostop) ', nostop','\'')
    cmd = paste0("timeout ", timeout," ", cmd)
  } else {
    cmd = paste0(stata_bin,' -b do "',file,'"', if(nostop) ', nostop')
  }

  start.time = Sys.time()

  res = system(cmd)
  end.time = Sys.time()
  runtime = as.numeric(Sys.time()-start.time)

  if (!is_empty(timeout) & !is_windows) {
    if (res != 0) {
      if (verbose) {
        cat(paste0("\n    stopped due to timeout (", timeout, " seconds.)\n"))
      }
      return(list(timeout=TRUE, runtime=runtime))
    }
  } else if (res!=0) {
    stop(paste0("There was an error (code ", res,") when running the Stata command:\n\n", cmd, "\n\nPossible you have to specify the path to the Stata binary by calling set_stata_paths(...)."),call. = FALSE)
  }
  if (verbose) {
    cat(paste0("\n    finished after ", round(runtime,2), " seconds.\n"))
  }
  return(list(timeout=FALSE, runtime=runtime))
}

set_stata_paths = function(stata_bin = NULL,ado_dirs=NULL, base_ado_dir = NULL, stata_dir=NULL, is_windows =  .Platform$OS.type == "windows") {
  if (is_windows) {
    if (!is.null(stata_dir)) {
      if (is.null(stata_bin)) {
        stata_bin = paste0(stata_dir,"/StataSE-64.exe")
      } else if (basename(stata_bin)==stata_bin) {
        stata_bin = paste0(stata_dir, "/", stata_bin)
      }
      if (is.null(base_ado_dir)) {
        base_ado_dir = file.path(stata_dir, "ado","base")
      }
    }
  }
  options(repbox.stata.paths = list(stata_bin=stata_bin, ado_dirs = ado_dirs, base_ado_dir = base_ado_dir))
}

check_stata_paths_and_ado = function(is_windows = .Platform$OS.type == "windows", check_base_ado_dir = FALSE, on_fail = c("error","warn","msg","silent")[1]) {
  restore.point("check_stata_paths_and_ado")

  p = getOption("repbox.stata.paths")
  if (on_fail=="error") {
    fail_fun = function(...) stop(paste0(...),call. = FALSE)
  } else if (on_fail=="warn") {
    fail_fun = function(...) warning(paste0(...))
  } else if (on_fail=="msg") {
    fail_fun = function(...) cat(paste0(...))
  } else {
    fail_fun = function(...) {}
  }

  if (is.null(p) & is_windows) {
    fail_fun("On windows you must set custom stata paths. Please call set_stata_paths")
    return(FALSE)
  }
  ok = TRUE
  stata_bin = get_stata_bin()
  if (basename(stata_bin)!=stata_bin) {
    if (!file.exists(stata_bin)) {
      ok = FALSE
      fail_fun(paste0("The stata binary ", stata_bin, " could not be found."))
    }
  }

  dir = ado_dirs = get_ado_dirs()
  if (!any(dir.exists(dir))) {
    ok = FALSE
    fail_fun(paste0("The modules ado directories ",paste0(dir, collapse=", "), " do not all exist."))
  }

  if (check_base_ado_dir) {
    dir = get_base_ado_dir()
    if (!dir.exists(dir)) {
      ok = FALSE
      fail_fun(paste0("The base ado directory ",dir, " does not exist."))
    }
  }


  if (ok) {
    required_ado = c("parmest.ado",basename(list_repbox_ado_files(only_required=TRUE)))
    initials = substring(required_ado,1,1)
    exists = rep(FALSE, length(required_ado))
    for (dir in ado_dirs) {
      exists = exists |
        file.exists(paste0(dir,"/",required_ado)) |
        file.exists(paste0(dir,"/",initials,"/",required_ado))
    }
    if (!all(exists)) {
      ok = FALSE
      msg = paste0("\nNot all repbox-specific Stata ado files can be found in your specified ado directories:\n\n  ", paste0(ado_dirs, collapse="\n  "), "\n\nPlease call copy_repbox_ado_files(ado_dir) to copy the required ado files.")
      fail_fun(msg)
    }
  }

  return(ok)
}

get_stata_bin = function(default = "stata-se") {
  res = getOption("repbox.stata.paths")$stata_bin
  if (is.null(res)) return(default)
  return(res)
}

is_empty = function (x) {
  if (is.null(x) | all(is.na(x)))
    return(TRUE)
  if (isTRUE(x == ""))
    return(TRUE)
  return(FALSE)
}

is.true = function (x)
{
  val = x == TRUE
  val[is.na(x)] = FALSE
  val
}
