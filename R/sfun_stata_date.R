sfun_stata_date = function(s, fmt, century_pivot = NULL) {
  restore.point("sfun_stata_date")

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
    stop("sfun_stata_date: Unsupported date format '", fmt, "'")
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

  # Apply Stata's century pivot logic for two-digit years
  if (grepl("%y", format_used)) { # Check if a two-digit year format was used
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
    
    # Reconstruct date with corrected year
    parsed_date = as.Date(paste(corrected_year, format(parsed_date, "%m-%d"), sep="-"))
  }

  # Stata's date origin: 01jan1960
  stata_epoch = as.Date("1960-01-01")

  # Calculate days since Stata epoch
  stata_date = as.numeric(parsed_date - stata_epoch)

  return(stata_date)
}

