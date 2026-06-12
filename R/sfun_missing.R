# Custom R implementation of Stata function missing

# Stata's missing() function checks for system missing (.) and extended missing values (.a to .z).
# In R, these are typically all read as NA by packages like `haven`.
# Additionally, for string variables, Stata's missing() treats empty strings ("") or strings containing only blanks as missing.
# Custom R implementation of Stata function missing

# Stata's missing() function checks for system missing (.) and extended missing values (.a to .z).
# In R, these are typically all read as NA by packages like `haven`.
# Additionally, for string variables, Stata's missing() treats empty strings ("") or strings containing only blanks as missing.
# Stata's missing() can take multiple arguments, returning 1 if any of them is missing.
sfun_missing = function(...) {
  # No restore.point here to avoid issues with ... in environments
  args = list(...)
  if (length(args) == 0) return(FALSE)

  check_missing = function(x) {
    if (is.numeric(x)) {
      return(is.na(x))
    } else if (is.character(x)) {
      # For character vectors, consider NA or empty/blank strings as missing
      return(is.na(x) | stringi::stri_trim_both(x) == "")
    }
    return(is.na(x)) # Default for other types
  }

  res = check_missing(args[[1]])
  if (length(args) > 1) {
    for (i in 2:length(args)) {
      res = res | check_missing(args[[i]])
    }
  }
  return(res)
}

# Example usage if you wanted to call this explicitly:
# data = data %>% dplyr::mutate(new_var = sfun_missing(old_var))
# However, direct translation to is.na() is preferred in generated code for simplicity.

