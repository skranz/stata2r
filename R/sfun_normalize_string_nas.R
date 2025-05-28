# Custom R implementation to normalize string NAs to empty strings ""
# This is to match Stata's behavior where missing string values are empty strings.

sfun_normalize_string_nas = function(df) {
  restore.point("sfun_normalize_string_nas")
  # Iterate over columns and apply conversion for character columns
  for (col_name in names(df)) {
    if (is.character(df[[col_name]])) {
      # Use base R for assignment to ensure robustness
      df[[col_name]][is.na(df[[col_name]])] = ""
    }
  }
  return(df)
}


