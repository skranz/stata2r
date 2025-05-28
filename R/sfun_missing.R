# Custom R implementation of Stata function missing

# Stata's missing() function checks for system missing (.) and extended missing values (.a to .z).
# In R, these are typically all read as NA by packages like `haven`.
# Additionally, for string variables, Stata's missing() treats empty strings ("") or strings containing only blanks as missing.
sfun_missing = function(x) {
  restore.point("sfun_missing")
  if (is.numeric(x)) {
    return(is.na(x))
  } else if (is.character(x)) {
    # For character vectors, consider NA or empty/blank strings as missing
    return(is.na(x) | stringi::stri_trim_both(x) == "")
  }
  return(is.na(x)) # Default for other types
}

# Example usage if you wanted to call this explicitly:
# data = data %>% dplyr::mutate(new_var = sfun_missing(old_var))
# However, direct translation to is.na() is preferred in generated code for simplicity.


