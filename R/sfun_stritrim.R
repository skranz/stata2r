# Custom R implementation for Stata's stritrim() behavior
# Stata: stritrim(s) removes leading/trailing spaces and replaces multiple internal spaces with one.

sfun_stritrim = function(s) {
  # Remove leading/trailing spaces
  s = stringi::stri_trim_both(s)
  # Replace multiple internal spaces with a single space
  s = stringi::stri_replace_all_regex(s, "\\s+", " ")
  return(s)
}


