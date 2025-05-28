sfun_stata_date = function(s, fmt, century_pivot = NULL) {
  restore.point("sfun_stata_date")

  # Apply sfun_stata_date_single to each element of s
  # vapply ensures the output type is numeric(1) for each element,
  # and the result is a numeric vector.
  vapply(s, sfun_stata_date_single, FUN.VALUE = numeric(1), fmt = fmt, century_pivot = century_pivot)
}

