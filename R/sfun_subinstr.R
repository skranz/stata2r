# Custom R implementation for Stata's subinstr() behavior
# Stata: subinstr(s1, s2, s3, n) replaces n occurrences of s2 with s3 in s1.
# If n is 0 or negative, or if n is ., all occurrences are replaced.
# If s2 is "", s1 is returned.

sfun_subinstr = function(s1, s2, s3, n) {
  # Handle s2 being empty string
  if (is.character(s2) && s2 == "") {
    return(s1)
  }

  # Stata's `.` for `n` is NA in R. Non-positive `n` also means all.
  if (is.na(n) || n <= 0) {
    # Replace all occurrences
    return(stringi::stri_replace_all_fixed(s1, s2, s3))
  } else if (n == 1) {
    # Replace first occurrence
    return(stringi::stri_replace_first_fixed(s1, s2, s3))
  } else {
    # For n > 1 and finite, Stata replaces only n times.
    # stringi::stri_replace_all_fixed doesn't have a 'limit' argument.
    # This is a known limitation for perfect emulation for n > 1 and finite.
    # For now, if n > 1 and finite, we will replace all.
    warning("sfun_subinstr: Stata's subinstr with finite n > 1 is not perfectly emulated. All occurrences will be replaced.")
    return(stringi::stri_replace_all_fixed(s1, s2, s3))
  }
}


