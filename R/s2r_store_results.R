# Stable storage helpers for Stata-style current e() and r() results

s2r_store_e_results = function(res) {
  restore.point("s2r_store_e_results")

  assign("stata_e_sample", NULL, envir = stata2r_env)
  assign("stata_e_N", NA_real_, envir = stata2r_env)
  assign("stata_e_r2", NA_real_, envir = stata2r_env)
  assign("stata_e_df_r", NA_real_, envir = stata2r_env)
  assign("stata_e_rmse", NA_real_, envir = stata2r_env)

  if (!is.null(res$e_sample)) {
    assign("stata_e_sample", res$e_sample, envir = stata2r_env)
  }
  if (!is.null(res$e_N)) {
    assign("stata_e_N", res$e_N, envir = stata2r_env)
  }
  if (!is.null(res$e_r2)) {
    assign("stata_e_r2", res$e_r2, envir = stata2r_env)
  }
  if (!is.null(res$e_df_r)) {
    assign("stata_e_df_r", res$e_df_r, envir = stata2r_env)
  }
  if (!is.null(res$e_rmse)) {
    assign("stata_e_rmse", res$e_rmse, envir = stata2r_env)
  }

  invisible(res)
}

s2r_store_r_results = function(res) {
  restore.point("s2r_store_r_results")

  assign("stata_r_N", NA_real_, envir = stata2r_env)
  assign("stata_r_mean", NA_real_, envir = stata2r_env)
  assign("stata_r_sd", NA_real_, envir = stata2r_env)
  assign("stata_r_min", NA_real_, envir = stata2r_env)
  assign("stata_r_max", NA_real_, envir = stata2r_env)
  assign("stata_r_sum", NA_real_, envir = stata2r_env)
  assign("stata_r_p50", NA_real_, envir = stata2r_env)

  if (!is.null(res$r_N)) {
    assign("stata_r_N", res$r_N, envir = stata2r_env)
  }
  if (!is.null(res$r_mean)) {
    assign("stata_r_mean", res$r_mean, envir = stata2r_env)
  }
  if (!is.null(res$r_sd)) {
    assign("stata_r_sd", res$r_sd, envir = stata2r_env)
  }
  if (!is.null(res$r_min)) {
    assign("stata_r_min", res$r_min, envir = stata2r_env)
  }
  if (!is.null(res$r_max)) {
    assign("stata_r_max", res$r_max, envir = stata2r_env)
  }
  if (!is.null(res$r_sum)) {
    assign("stata_r_sum", res$r_sum, envir = stata2r_env)
  }
  if (!is.null(res$r_p50)) {
    assign("stata_r_p50", res$r_p50, envir = stata2r_env)
  }

  invisible(res)
}
