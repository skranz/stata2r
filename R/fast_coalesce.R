#' Fast alternative to fast_coalesce for simple vectors
#'
#' Avoids the heavy vctrs and S3 dispatch overhead of fast_coalesce.
#' Ideal for inline use in loops, regex wrappers, and generated code.
fast_coalesce = function(x, y) {
  if (is.null(x)) return(y)

  idx = is.na(x)
  if (any(idx)) {
    if (length(y) == 1L) {
      x[idx] = y
    } else {
      x[idx] = y[idx]
    }
  }
  x
}
