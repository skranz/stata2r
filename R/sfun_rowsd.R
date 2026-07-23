# Custom R implementation for row-wise standard deviation
sfun_rowsd = function(x, na.rm = TRUE) {
  restore.point("sfun_rowsd")
  x_mat = as.matrix(x)

  if (na.rm) {
    n = base::rowSums(!is.na(x_mat))
    means = base::rowMeans(x_mat, na.rm = TRUE)
    devs = x_mat - means
    vars = base::rowSums(devs^2, na.rm = TRUE) / (n - 1)
    vars[n < 2] = NA_real_
    return(sqrt(vars))
  } else {
    n = ncol(x_mat)
    means = base::rowMeans(x_mat, na.rm = FALSE)
    devs = x_mat - means
    vars = base::rowSums(devs^2, na.rm = FALSE) / (n - 1)
    return(sqrt(vars))
  }
}
