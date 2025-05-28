# Custom R implementation for Stata's string() behavior
# Stata: string(x) converts numeric x to string.
# Missing numeric values (.) are converted to an empty string ("").

sfun_string = function(x) {
  restore.point("sfun_string")
  # Convert to character first
  res = as.character(x)

  # Stata specific: missing numeric values become empty string.
  # This applies to R's NA_real_ (for numeric) and NA_character_ (for character).
  res[is.na(res)] = ""

  return(res)
}

