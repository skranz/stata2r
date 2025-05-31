# Custom R implementation for Stata's month() behavior
# Stata: month(date_value) returns the month (1-12) of a Stata date value.

sfun_month = function(stata_date_values) {
  restore.point("sfun_month")
  # Convert Stata date (numeric days since 1960-01-01) to R Date object.
  # as.Date handles NA values correctly.
  r_dates = as.Date(stata_date_values, origin = "1960-01-01")
  
  # Extract month as numeric.
  # format() returns a character string, so convert to numeric.
  # This will result in NA for any invalid date conversions.
  return(as.numeric(format(r_dates, "%m")))
}


