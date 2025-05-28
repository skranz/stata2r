# Custom R implementation for Stata's strpos() behavior
# Stata: strpos(haystack, needle) returns the first position of needle in haystack.
# Returns 0 if needle is not found.

sfun_strpos = function(haystack, needle) {
  restore.point("sfun_strpos")
  # stringi::stri_locate_first_fixed returns a matrix [start, end]
  # or [NA, NA] if not found. We need the start position.
  loc = stringi::stri_locate_first_fixed(haystack, needle)
  start_pos = loc[,1]
  return(ifelse(is.na(start_pos), 0L, as.integer(start_pos)))
}

# Example Usage:
# sfun_strpos("this is a test", "is")  # Expected: 3
# sfun_strpos("this is a test", "not") # Expected: 0
# sfun_strpos(c("apple", "banana", "apricot"), "ap") # Expected: c(1, 0, 1)

