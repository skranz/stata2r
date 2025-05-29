# Custom R implementation for Stata's year() behavior
# Stata: year(date_value) returns the year of a Stata date value.

sfun_year = function(stata_date_values) {
  restore.point("sfun_year")
  # Convert Stata date (numeric days since 1960-01-01) to R Date object.
  # as.Date handles NA values correctly.
  r_dates = as.Date(stata_date_values, origin = "1960-01-01")

  # Extract year as numeric.
  # format() returns a character string, so convert to numeric.
  # This will result in NA for any invalid date conversions.
  return(as.numeric(format(r_dates, "%Y")))
}

