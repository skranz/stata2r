# Custom R implementation for Stata's qofd() behavior
# Stata: qofd(date_value) returns the quarter of the year (1-4) of a Stata date value.

sfun_qofd = function(stata_date_values) {
  restore.point("sfun_qofd")
  # Convert Stata date (numeric days since 1960-01-01) to R Date object.
  # as.Date handles NA values correctly.
  r_dates = as.Date(stata_date_values, origin = "1960-01-01")
  
  # Extract month (1-12)
  months = as.numeric(format(r_dates, "%m"))
  
  # Calculate quarter (1-4)
  # ceiling(month / 3) correctly maps 1-3 to 1, 4-6 to 2, etc.
  quarters = ceiling(months / 3)
  
  # Ensure NA for invalid dates
  quarters[is.na(r_dates)] = NA_real_
  
  return(quarters)
}

