# Custom R implementation to normalize string NAs to empty strings ""
# This is to match Stata's behavior where missing string values are empty strings.

sfun_normalize_string_nas = function(df) {
  restore.point("sfun_normalize_string_nas")
  # Apply to each character column
  df = dplyr::mutate(df, dplyr::across(where(is.character), ~dplyr::if_else(is.na(.), "", .)))
  return(df)
}

