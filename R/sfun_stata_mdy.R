# Custom R implementation for Stata's mdy() behavior
# Stata: mdy(M,D,Y) returns the number of days since 01jan1960.

sfun_stata_mdy = function(M, D, Y) {
  restore.point("sfun_stata_mdy")
  # Create a string in "YYYY-MM-DD" format that as.Date can parse
  date_str = paste0(Y, "-", sprintf("%02d", M), "-", sprintf("%02d", D))
  
  # Convert to R Date object. Suppress warnings for invalid dates (e.g., Feb 30).
  # Invalid dates will result in NA.
  r_date = suppressWarnings(as.Date(date_str, format = "%Y-%m-%d"))
  
  # Return numeric value as days since 1960-01-01 (Stata's epoch)
  # Stata's `date()` and `mdy()` functions define 01jan1960 as 0.
  # R's `as.numeric(as.Date("1960-01-01") - as.Date("1960-01-01"))` correctly yields 0.
  stata_date_value = as.numeric(r_date - as.Date("1960-01-01"))
  
  return(stata_date_value)
}

