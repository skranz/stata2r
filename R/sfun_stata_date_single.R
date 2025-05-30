# Helper function for sfun_stata_date (non-vectorized core logic)
sfun_stata_date_single = function(s, fmt, century_pivot = NULL) {
  restore.point("sfun_stata_date_single")

  # Handle NA input
  if (is.na(s)) return(NA_real_)

  # Stata's default century for date() is 2000 if not specified (e.g., date("1/1/60", "MDY") -> 2060)
  # The 'century_pivot' argument shifts this.
  actual_century_pivot = if (is.null(century_pivot)) 2000 else as.numeric(century_pivot)

  # Define common R format strings based on Stata's fmt, including variants with/without separators and 2/4 digit years
  r_formats = switch(tolower(fmt),
    "ymd" = c("%Y%m%d", "%Y-%m-%d", "%Y/%m/%d", "%y%m%d", "%y-%m-%d", "%y/%m/%d"),
    "mdy" = c("%m%d%Y", "%m-%d-%Y", "%m/%d/%Y", "%m%d%y", "%m-%d-%y", "%m/%d/%y"),
    "dmy" = c("%d%m%Y", "%d-%m-%Y", "%d/%m/%Y", "%d%m%y", "%d-%m-%y", "%d/%m/%y"),
    stop("sfun_stata_date_single: Unsupported date format '", fmt, "'")
  )

  parsed_date = as.Date(NA_character_)
  format_used = NA_character_

  # Try parsing with multiple formats until successful
  for (f in r_formats) {
    temp_date = suppressWarnings(as.Date(s, format = f))
    if (!is.na(temp_date)) {
      parsed_date = temp_date
      format_used = f
      break
    }
  }

  # If parsing failed, return NA
  if (is.na(parsed_date)) {
    return(NA_real_)
  }

  # Apply Stata's century pivot logic for two-digit years if the format *type* allows it.
  # This is a more robust check than just `grepl("%y", format_used)` as Stata's `date()`
  # function applies this logic based on the format's capacity for 2-digit years,
  # even if a 4-digit year is provided in the input string.
  if (is_stata_fmt_allowing_2_digit_year(fmt)) {
    current_year_full = as.numeric(format(parsed_date, "%Y"))
    current_year_two_digits = current_year_full %% 100
    
    pivot_year_last_two_digits = actual_century_pivot %% 100
    
    if (current_year_two_digits >= pivot_year_last_two_digits) {
      # If two-digit year is >= pivot_year_last_two_digits, it belongs to the previous century (e.g., 19xx for 2050 pivot)
      corrected_year = floor(actual_century_pivot / 100) * 100 - 100 + current_year_two_digits
    } else {
      # If two-digit year is < pivot_year_last_two_digits, it belongs to the current century (e.g., 20xx for 2050 pivot)
      corrected_year = floor(actual_century_pivot / 100) * 100 + current_year_two_digits
    }
    
    # Reconstruct date with corrected year if needed (only if corrected_year is different)
    if (corrected_year != current_year_full) {
      parsed_date = as.Date(paste(corrected_year, format(parsed_date, "%m-%d"), sep="-"))
    }
  }

  # Return numeric value as days since 1960-01-01 (Stata's epoch)
  # Stata's `date()` and `mdy()` functions define 01jan1960 as 0.
  # R's `as.numeric(as.Date("1960-01-01") - as.Date("1960-01-01"))` correctly yields 0.
  stata_date = as.numeric(parsed_date - as.Date("1960-01-01"))

  return(stata_date)
}

