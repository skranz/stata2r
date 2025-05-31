# Custom R implementation for Stata's dow() behavior
# Stata: dow(date_value) returns the day of the week (0-6) of a Stata date value.
# 0 = Sunday, 1 = Monday, ..., 6 = Saturday.

sfun_dow = function(stata_date_values) {
  restore.point("sfun_dow")
  # Convert Stata date (numeric days since 1970-01-01 for consistency with R's internal Date representation) to R Date object.
  # as.Date handles NA values correctly.
  r_dates = as.Date(stata_date_values, origin = "1970-01-01")
  
  # Extract day of the week as numeric (0 for Sunday, ..., 6 for Saturday).
  # format() with "%w" provides this behavior.
  # This will result in NA for any invalid date conversions.
  return(as.numeric(format(r_dates, "%w")))
}

