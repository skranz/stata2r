# Custom R implementation for Stata's compress logic for numeric types.
# Attempts to convert numeric vectors to integer if all non-NA values are integers
# and within the range of R's integer type.

sfun_compress_col_type = function(x) {
  restore.point("sfun_compress_col_type")
  if (is.numeric(x)) {
    # Check if all non-NA values are integers and within R's integer range
    # Also check if there are any non-NA values to avoid issues with empty vectors or all NAs
    if (length(x[!is.na(x)]) > 0 && all(x == floor(x), na.rm = TRUE) && all(x >= -.Machine$integer.max & x <= .Machine$integer.max, na.rm = TRUE)) {
      return(as.integer(x))
    }
  }
  return(x)
}

