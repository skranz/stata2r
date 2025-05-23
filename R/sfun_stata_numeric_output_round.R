sfun_stata_numeric_output_round = function(x) {
  # Apply to numeric vectors only
  if (!is.numeric(x)) return(x)

  # Stata's 'float' type has about 7 decimal digits of precision.
  # To accurately compare R's double-precision results with Stata's float-precision results,
  # we round to a consistent number of decimal places using Stata's rounding rule.
  # Using 1e-8 as the unit for sfun_stata_round to ensure consistent rounding to 8 decimal places.
  return(sfun_stata_round(x, 1e-8))
}

