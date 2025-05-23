sfun_stata_numeric_output_round = function(x) {
  # Apply to numeric vectors only
  if (!is.numeric(x)) return(x)

  # Stata's 'float' type (default for 'generate') and display formats
  # can lead to values being stored with certain precision or rounding.
  # Observed behavior for 'log(i)' suggests:
  # 1. Rounding to 2 decimal places for most values.
  # 2. Snapping to the nearest integer for values very close to an integer.

  # Step 1: Apply primary rounding, e.g., to 2 decimal places.
  # Uses sfun_stata_round for Stata's specific rounding rule (away from zero for .5).
  x_rounded_primary = sfun_stata_round(x, 0.01)

  # Step 2: For values that are very close to an integer, snap to that integer.
  # The epsilon (0.005) is chosen based on observed differences (e.g., log(20) ~ 2.9957 -> 3).
  epsilon = 0.005
  is_close_to_integer = abs(x - round(x)) < epsilon

  # If close to integer, round to nearest integer using Stata's rounding rule.
  # Otherwise, use the result from primary rounding.
  x_final = ifelse(is_close_to_integer, sfun_stata_round(x, 1), x_rounded_primary)

  # Ensure the return type is numeric (double), as Stata's float maps to R's numeric
  return(as.numeric(x_final))
}

