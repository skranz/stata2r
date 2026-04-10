# FILE: R/sfun_inrange.R

#' Custom R implementation for Stata's inrange() function
#' Stata: inrange(z, a, b) returns 1 if a <= z <= b and 0 otherwise.
#' In Stata, numeric missing (.) acts as positive infinity.
#' Thus, missing z is treated as +Inf, missing a as -Inf, and missing b as +Inf.
#'
#' Note:
#' We intentionally do not call restore.point() here. This helper is often
#' evaluated inside dplyr data-masked environments, where restore.point()
#' can fail while trying to inspect active bindings.
sfun_inrange = function(z, a, b) {
  n = length(z)
  if (n == 0) return(logical(0))

  recycle_arg = function(x) {
    if (length(x) == n) return(x)
    if (length(x) == 1L) return(rep(x, n))
    stop("sfun_inrange: each bound must have length 1 or length(z).")
  }

  a = recycle_arg(a)
  b = recycle_arg(b)

  z_num = suppressWarnings(as.numeric(z))
  a_num = suppressWarnings(as.numeric(a))
  b_num = suppressWarnings(as.numeric(b))

  z_num[is.na(z_num)] = Inf
  a_num[is.na(a_num)] = -Inf
  b_num[is.na(b_num)] = Inf

  res = (z_num >= a_num) & (z_num <= b_num)
  res[is.na(res)] = FALSE

  return(res)
}
