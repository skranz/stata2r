# Custom R implementation for Stata's polymorphic '+' operator.
# Stata's '+' operator performs numeric addition if both operands are numeric.
# If one or both operands are strings, it performs string concatenation (after converting numeric operands to string).
# Stata's missing value handling for '+' is specific:
# numeric + numeric: standard addition, NA if any operand is NA.
# string + string: concatenation.
# numeric + string: numeric converted to string, then concatenation.
# . (missing numeric) + string: . converted to "" for concatenation. Result is string.
# string + . (missing numeric): . converted to "" for concatenation. Result is string.
# . (missing numeric) + . (missing numeric): . Result is NA (numeric).

sfun_stata_add = function(x, y) {
  # If both are numeric, perform numeric addition.
  # This correctly handles NA + NA = NA, numeric + NA = NA.
  if (is.numeric(x) && is.numeric(y)) {
    return(x + y)
  } else {
    # If not both numeric, assume string concatenation.
    # Convert both to character.
    x_char = as.character(x)
    y_char = as.character(y)

    # Stata specific: missing numeric (NA in R) becomes empty string for concatenation.
    # This is a heuristic as we don't have original type info, assuming if it's numeric NA, it was Stata's '.'
    x_val = if (is.numeric(x) && is.na(x)) "" else x_char
    y_val = if (is.numeric(y) && is.na(y)) "" else y_char

    # If either original value was NA_character_ (from a Stata string missing), the result should be NA_character_
    # This needs to check the `as.character()` result for NA, not just `is.na(x)`.
    # `paste0` converts NA to "NA" by default. We need to prevent this if the original was NA_character_.
    if (is.na(x_char) || is.na(y_char)) {
        return(NA_character_)
    } else {
        return(paste0(x_val, y_val))
    }
  }
}

