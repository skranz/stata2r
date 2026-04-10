# FILE: R/sfun_inlist.R

#' Custom R implementation for Stata's inlist() function
#' Stata: inlist(z, a, b, ...) returns 1 if z is equal to any of a, b, ... and 0 otherwise.
#' It evaluates row-wise when vector arguments are passed, and correctly matches missing values.
#'
#' Note:
#' We intentionally do not call restore.point() here. This helper is often
#' evaluated inside dplyr data-masked environments, where restore.point()
#' can fail while trying to inspect active bindings.
sfun_inlist = function(z, ...) {
  args = list(...)
  n = length(z)

  if (length(args) == 0) return(rep(FALSE, n))
  if (n == 0) return(logical(0))

  recycle_arg = function(x) {
    if (length(x) == n) return(x)
    if (length(x) == 1L) return(rep(x, n))
    stop("sfun_inlist: each comparison argument must have length 1 or length(z).")
  }

  res = rep(FALSE, n)

  for (arg in args) {
    arg = recycle_arg(arg)

    if (is.character(z)) {
      z_cmp = as.character(z)
      arg_cmp = as.character(arg)
      match_idx = (z_cmp == arg_cmp) | (is.na(z_cmp) & is.na(arg_cmp))
    } else {
      match_idx = (z == arg) | (is.na(z) & is.na(arg))
    }

    match_idx[is.na(match_idx)] = FALSE
    res = res | match_idx
  }

  return(res)
}
