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
  restore.point("sfun_stata_add")
  # If both are numeric, perform numeric addition.
  # This correctly handles NA + NA = NA, numeric + NA = NA.
  if (is.numeric(x) && is.numeric(y)) {
    return(x + y)
  } else {
    # If not both numeric, assume string concatenation.
    # Convert both to character.
    x_char = as.character(x)
    y_char = as.character(y)

    # Stata specific: missing numeric (NA in R) and string missing (NA_character_ in R)
    # become empty string for concatenation.
    x_val = x_char
    y_val = y_char
    x_val[is.na(x_val)] = ""
    y_val[is.na(y_val)] = ""

    return(paste0(x_val, y_val))
  }
}

