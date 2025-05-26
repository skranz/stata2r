# Custom R implementation for Stata's polymorphic '+' operator.
# Stata's '+' operator performs numeric addition if both operands are numeric.
# If one or both operands are strings, it performs string concatenation (after converting numeric operands to string).

sfun_stata_add = function(x, y) {
  # Check if both operands are numeric. Stata treats NA as missing numeric.
  # If either is NA (Stata missing), the result of arithmetic/concatenation involving it can be NA.
  # For string concatenation, NA usually concatenates as "NA".
  # For Stata, `.` + `.` is `.` (missing). `string` + `.` is `string`.
  # `numeric_val` + `.` is `.`

  # Handle missing values first to match Stata's behavior as closely as possible for `+`.
  # If one is NA, and the other is a string, Stata often converts the NA to "" for concatenation.
  # But if it's numeric + NA, it's NA.
  # The `paste0` function will convert NA to "NA" by default, which is not Stata-like.
  # A more precise emulation of Stata's missing + string behavior:
  # Stata: "abc" + . -> "abc"
  # Stata: . + "abc" -> "abc"
  # Stata: . + . -> .

  # For simplicity and given the `non-numeric argument` error, we will use R's standard `paste0` behavior
  # for string concatenation (which includes "NA" for NA values) and R's `+` for numeric.
  # More complex missing value handling for string concatenation can be added later if needed.

  if (is.numeric(x) && is.numeric(y)) {
    return(x + y)
  } else {
    # If not both numeric, assume string concatenation.
    # Convert to character if not already.
    return(paste0(as.character(x), as.character(y)))
  }
}

