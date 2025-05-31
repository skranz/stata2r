# Custom R implementation for utility functions related to Stata date formats.

is_stata_fmt_allowing_2_digit_year = function(fmt) {
  # Stata formats that allow 2-digit years (YY) implicitly or explicitly.
  # This function checks for common daily date formats where Stata's `date()`
  # function might apply century logic even if the input string contains a 4-digit year.
  # These are the formats that typically have a 'Y' or 'y' component for the year.
  tolower(fmt) %in% c("ymd", "mdy", "dmy", "dy", "my", "yd", "yw", "wq", "wm", "wd")
}


