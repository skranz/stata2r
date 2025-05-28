# Custom R implementation for Stata's qofd() behavior
# Stata: qofd(date_value) returns the quarter of the year (1-4) of a Stata date value.
#
# Reverting previous modification to match Stata's documented qofd() behavior (1-4).
# If the test data requires Stata's q() function (quarterly date value), the test data or
# the Stata script should be updated to use q().

sfun_qofd = function(stata_date_values) {
  restore.point("sfun_qofd")
  # Convert Stata date (numeric days since 1960-01-01) to R Date object.
  # as.Date handles NA values correctly.
  r_dates = as.Date(stata_date_values, origin = "1960-01-01")
  
  # Extract month
  months = as.numeric(format(r_dates, "%m"))
  
  # Calculate quarter of the year (1-4)
  quarters_of_year = ceiling(months / 3)
  
  # Ensure NA for invalid dates
  quarters_of_year[is.na(r_dates)] = NA_real_
  
  return(quarters_of_year)
}

