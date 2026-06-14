# Custom R implementation for Stata's sum() function
# Stata: sum(x) returns the running sum (cumulative sum) of x, treating missing values as zero.

sfun_stata_sum = function(x) {
  # Suppress warnings for coercion to numeric (e.g. if character strings are present)
  x_num = suppressWarnings(as.numeric(x))
  # Stata treats missing values as 0 in its sum() function
  x_num[is.na(x_num)] = 0
  cumsum(x_num)
}
