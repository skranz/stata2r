examples = function() {
  do_file = "C:/libraries/aicoder/stata2r/inst/cases/custom_1/do1.do"
  aic_stata2r_do_test(aic=NULL, test_dir = "~/aicoder/stata2r/aicoder_work/tests/do1", data_dir = "~/aicoder/stata2r/inst/cases/do1/do_data")
}

aic_stata2r_do_test = function(aic, test_dir, data_dir, data_prefix="") {
  restore.point("aic_stata2r_do_test")
  txt = capture.output(err<-try(aic_stata2r_do_test_inner(test_dir, data_dir, data_prefix), silent=TRUE))
  log = out_and_err_txt(txt, err)
  cat(log)
  has_err = is(err, "try-error") | isTRUE(err==FALSE)

  #test_log= list(ok=!has_err,test_name=basename(test_dir), msg="", log=log)
  aic = aic_add_test(aic, test_name=basename(test_dir), ok=!has_err, log=log)
  aic
}

aic_stata2r_do_test_inner = function(test_dir, data_dir, data_prefix="") {
  restore.point("aic_stata2r_do_test_inner")
  setwd(test_dir)
  library(stata2r)
  # Explicitly load dependencies for the test environment
  library(collapse)
  library(dplyr)
  library(stringi)
  library(haven)
  library(tidyr) # For reshape
  library(restorepoint) # If used by translated code or framework
  library(readr) # For destring

  # do code that will be translated
  do_code = readLines("do1.do", warn=FALSE)
  #cat(do_code, sep="\n")


  # transforms do_code to a dataframe with
  # one row for each code line possible
  # perform some preparsing
  # the field do_code should contain the original code
  cat("\ncmd_df = do_parse(do_code)")
  cmd_df = do_parse(do_code)

  # will add field "do_translate"
  # if FALSE the stata command does not modify the data set
  # and can be ignored
  cat("\ncmd_df = mark_data_manip_cmd(cmd_df)\n")
  cmd_df = mark_data_manip_cmd(cmd_df)
  cat("\nstr(cmd_df)\n")
  print(str(cmd_df))

  r_li = vector("list", NROW(cmd_df))

  cat("\n---\n#Translate to R commands\n")
  i = 1
  for (i in seq_along(r_li)) {
    #cat("\n", i,"of", length(r_li), "translate", cmd_df$do_code[[i]],"\n")
    cat("\ndo: ", cmd_df$do_code[[i]],"\n")
    cmd_obj = cmd_df[i,]
    r_obj = do_cmd_to_r(cmd_obj=cmd_obj,line=i, cmd_df=cmd_df)
    #print(str(r_obj))
    if (isTRUE(cmd_df$do_translate[i])) {
      cat("R: ", r_obj$r_code,"\n")
    } else {
      cat("  no data manipulation command\n")
    }
    r_li[[i]] = r_obj # Ensure r_li is always populated, even if command is not translated
  }
  r_df = bind_rows(r_li)

  env = new.env(parent=globalenv())

  cat("\n---\n# Run R commands and check generated data sets\n\n")
  i_df_loop = 1
  log_str = NULL
  for (i_df_loop in seq_len(NROW(r_df))) {
    r_code = r_df$r_code[[i_df_loop]]
    original_stata_line_num = r_df$line[[i_df_loop]] # Get original line number from r_df
    do_code_original = r_df$do_code[[i_df_loop]] # Get original do code for logging

    # If r_code is NA_character_ (meaning it was not translated/skipped), use a no-op R code
    if (is.na(r_code)) {
      cat("\n", original_stata_line_num, ": Skipping non-data manipulation command: ", do_code_original, "\n")
      r_code = "# No-op: Data unchanged from previous step by this command."
    }

    res = aicoder::run_with_log(code_str=r_code, env=env)
    cat("\n", original_stata_line_num, "R: ", r_code, "\n") # Print R code being run
    cat(res$log) # Print execution log

    if (res$has_error) {
      cat("\nError executing R code for Stata line ", original_stata_line_num, ": ", res$log, "\n")
      return(FALSE)
    }

    r_data = env[["data"]]
    if (!is.null(r_data)) {
      dat_file = file.path(data_dir, paste0(data_prefix, original_stata_line_num, ".dta")) # Use original line number for comparison
      do_data = haven::read_dta(dat_file)
      comp = compare_df(r_data, do_data)
      if (!comp$identical) {
        cat("\nError: After Stata line ", original_stata_line_num, ", R data set differs from Stata reference.\n")
        cat("\nR data set:\n")
        print(str(r_data))
        cat("\nStata version:\n")
        print(str(do_data))
        cat("\nDifferences:")
        print(str(comp))
        return(FALSE)
      }
    } else {
      cat("\nError: Data 'data' is NULL after Stata line ", original_stata_line_num, "\n")
      return(FALSE)
    }
  }
  return(TRUE)
}

aic_stata2r_eval_next_r_line = function(i, r_df, env) {

}

aic_stata2r_eval_example = function(do_file) {
  library(stata2r)
  do_dir = dirname(do_file)
  do_text = readLines(do_file)
  r_code = do_to_r(do_code)

  library(stata2r)
  library(aicoder)

}


out_and_err_txt = function(out, err=NULL) {
  if (is(err,"try-error")) {
    out = c(out,as.character(err))
  }
  paste0(out, collapse="\n")
}

compare_df = function(df1, df2,
                      tol = .Machine$double.eps^0.5,  # numeric tolerance
                      ignore_col_order = FALSE,
                      ignore_row_order = FALSE,
                      sample_n_diff = 5) {            # max rows to show per column

  # ---- basic structure checks ----
  if (!is.data.frame(df1) || !is.data.frame(df2))
    stop("Both inputs must be data frames.")

  if(identical(df1, df2)) return(list(identical=TRUE))

  if (ignore_col_order) {
    df1 = df1[, sort(names(df1)), drop = FALSE]
    df2 = df2[, sort(names(df2)), drop = FALSE]
  }
  if (ignore_row_order) {
    df1 = dplyr::arrange(df1, dplyr::across(dplyr::everything()))
    df2 = dplyr::arrange(df2, dplyr::across(dplyr::everything()))
  }

  out = list(identical=FALSE)

  # ---- dimension and column checks ----
  if (nrow(df1) != nrow(df2))
    out$row_count = c(df1 = nrow(df1), df2 = nrow(df2))

  missing_in_df1 = setdiff(names(df2), names(df1))
  missing_in_df2 = setdiff(names(df1), names(df2))
  if (length(missing_in_df1) + length(missing_in_df2) > 0)
    out$column_mismatch = list(missing_in_df1 = missing_in_df1,
                               missing_in_df2 = missing_in_df2)

  common_cols = intersect(names(df1), names(df2))

  # ---- class / type mismatches ----
  type_df = data.frame(col = common_cols,
                       class_df1 = vapply(df1[common_cols], class, character(1)),
                       class_df2 = vapply(df2[common_cols], class, character(1)),
                       stringsAsFactors = FALSE)
  type_diff = type_df[type_df$class_df1 != type_df$class_df2, ]
  if (nrow(type_diff) > 0)
    out$type_mismatch = type_diff

  # ---- value‐level comparison ----
  value_diffs = lapply(common_cols, function(cl) {
    v1 = df1[[cl]]
    v2 = df2[[cl]]

    # numeric columns need tolerance
    if (is.numeric(v1) && is.numeric(v2)) {
      neq = abs(v1 - v2) > tol | xor(is.na(v1), is.na(v2))
    } else {
      neq = v1 != v2 | xor(is.na(v1), is.na(v2))
    }
    which(neq)
  })
  names(value_diffs) = common_cols
  value_diffs = value_diffs[lengths(value_diffs) > 0]

  if (length(value_diffs) > 0) {
    # build a compact summary with at most sample_n_diff rows per column
    sampler = function(idx, cl) {
      head_idx = head(idx, sample_n_diff)
      data.frame(row = head_idx,
                 column = cl,
                 df1_value = df1[[cl]][head_idx],
                 df2_value = df2[[cl]][head_idx],
                 stringsAsFactors = FALSE)
    }
    diff_tbl = do.call(rbind, Map(sampler, value_diffs, names(value_diffs)))
    # tidy row names
    rownames(diff_tbl) = NULL
    out$value_mismatch = diff_tbl
  }

  # ---- return decision ----
  if (length(out) <= 1) {
    return(list(identical=TRUE))
  }
  out
}

