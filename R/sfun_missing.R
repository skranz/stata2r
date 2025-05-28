# Custom R implementation of Stata function missing

# Stata's missing() function checks for system missing (.) and extended missing values (.a to .z).
# In R, these are typically all read as NA by packages like `haven`.
# Therefore, `is.na()` is usually sufficient.
# This function is provided for completeness as requested.
sfun_missing = function(x) {
  restore.point("sfun_missing")
  return(is.na(x))
}

# Example usage if you wanted to call this explicitly:
# data = data %>% dplyr::mutate(new_var = sfun_missing(old_var))
# However, direct translation to is.na() is preferred in generated code for simplicity.

