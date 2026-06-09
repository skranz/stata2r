# FILE: R/sfun_index.R

#' Custom R implementation for Stata's row indexing var[index]
#' In Stata, if index <= 0 or index > _N, the result is missing (.).
#' This function evaluates the index and safely extracts values,
#' returning NA for out-of-bounds indices instead of throwing an R error.
sfun_index = function(x, idx) {
  # To avoid issues with data masked environments, we don't call restore.point
  n = length(x)
  if (n == 0) return(x)

  idx = suppressWarnings(as.integer(idx))

  if (length(idx) == 1L) {
    if (is.na(idx) || idx < 1L || idx > n) {
      # Extracting with NA ensures the correct NA type is returned
      res = x[NA_integer_]
      return(rep(res, n))
    } else {
      res = x[idx]
      return(rep(res, n))
    }
  }

  if (length(idx) != n) {
    stop("sfun_index: length of idx must be 1 or equal to length of x.")
  }

  # Replace invalid indices with NA so R extracts NA seamlessly
  idx[is.na(idx) | idx < 1L | idx > n] = NA_integer_
  return(x[idx])
}
