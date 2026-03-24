# FILE: stata2r/R/sfun_ts_utils.R

#' Safe wrapper for collapse::flag explicitly checking vector length
sfun_flag = function(x, n = 1, ...) {
  if (length(x) <= abs(n)) return(rep(NA_real_, length(x)))
  collapse::flag(x, n = n, ...)
}

#' Safe wrapper for collapse::fdiff explicitly checking vector length constraint
sfun_fdiff = function(x, n = 1, diff = 1, ...) {
  if (length(x) <= abs(n * diff)) return(rep(NA_real_, length(x)))
  collapse::fdiff(x, n = n, diff = diff, ...)
}
