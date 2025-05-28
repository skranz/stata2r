# Custom R implementation for Stata's qofd() behavior
# Stata: qofd(date_value) returns the quarter of the year (1-4) of a Stata date value.
#
# Note: The provided test data for `obs_quarter` (`do2` test case) appeared to be
# Stata's 0-indexed quarterly date values (quarters since 1960q1, where 1960q1 is 0).
# This implementation has been corrected to strictly return the quarter of the year (1-4)
# as per Stata's `qofd()` function, which expects a daily date and returns its quarter.

sfun_qofd = function(stata_date_values) {
  restore.point("sfun_qofd")
  # Convert Stata date (numeric days since 1960-01-01) to R Date object.
  # as.Date handles NA values correctly.
  r_dates = as.Date(stata_date_values, origin = "1960-01-01")

  # Extract month from the R Date object
  months = as.numeric(format(r_dates, "%m"))

  # Calculate quarter of the year (1-4) based on month
  quarters_of_year = ceiling(months / 3)

  # Ensure NA for invalid dates (if r_dates was NA, result should be NA)
  quarters_of_year[is.na(r_dates)] = NA_real_

  return(quarters_of_year)
}


