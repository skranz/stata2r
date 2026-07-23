# FILE: R/sfun_mod.R

#' Custom R implementation for Stata's mod() function
#' Stata: mod(x,y) returns x - y * int(x/y), the remainder of x/y truncating towards zero.
#' Note that R's %% operator evaluates to x - y * floor(x/y), which differs for negative values.
sfun_mod = function(x, y) {
  # We intentionally do not call restore.point() here because it is often
  # evaluated inside dplyr data-masked environments.

  x_num = suppressWarnings(as.numeric(x))
  y_num = suppressWarnings(as.numeric(y))

  res = x_num - y_num * trunc(x_num / y_num)
  # Stata defines mod(x, 0) as missing
  res[y_num == 0] = NA_real_

  return(res)
}
