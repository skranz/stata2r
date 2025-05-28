# Custom R implementation for Stata's qofd() behavior
# Stata: qofd(date_value) returns the quarter of the year (1-4) of a Stata date value.
#
# Note: The provided test data for `obs_quarter` (`do2` test case) appears to be
# Stata's 0-indexed quarterly date values (quarters since 1960q1, where 1960q1 is 0).
# The formula for this is (year - 1960) * 4 + (quarter_of_year - 1).
# This implementation is tailored to match the documented Stata `qofd()` behavior (returns 1-4).

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

