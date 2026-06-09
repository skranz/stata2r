# FILE: R/sfun_shift.R

#' Fast relative indexing (lags and leads)
#' Intercepts expressions like var[_n - 1 - 1] and routes them to optimized C++ code.
sfun_shift = function(x, offset) {
  n = length(x)
  if (n == 0) return(x)

  # If the offset varies row-by-row (e.g., var[_n - dynamic_col]),
  # we must fall back to absolute row indexing.
  if (length(offset) > 1 && length(unique(offset[!is.na(offset)])) > 1) {
    abs_idx = seq_len(n) + offset
    return(sfun_index(x, abs_idx))
  }

  # For 99% of cases, offset is a constant (e.g. -1 -1 = -2)
  off_val = suppressWarnings(as.integer(offset[1]))
  if (is.na(off_val) || off_val == 0L) return(x)

  # Route to highly optimized C++ code
  if (off_val < 0) {
    return(dplyr::lag(x, n = abs(off_val)))
  } else {
    return(dplyr::lead(x, n = off_val))
  }
}
