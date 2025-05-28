# Custom R implementation for Stata's round() behavior
# Stata: round(x) is equivalent to round(x,1)
# Stata: round(x,y) rounds x to the nearest multiple of y.
# For .5 cases, Stata rounds away from zero. (e.g. round(2.5,1)=3, round(-2.5,1)=-3)

sfun_stata_round = function(x, unit = 1) {
  # Ensure unit is positive, Stata's unit is always > 0
  if (any(unit <= 0, na.rm = TRUE)) {
    stop("sfun_stata_round: rounding unit must be positive.")
  }
  # Standard R NA handling: if x or unit is NA, result is NA
  # Using vectorized ifelse for NA handling implicitly
  return(ifelse(is.na(x) | is.na(unit), NA, sign(x) * floor(abs(x)/unit + 0.5) * unit))
}

# Example Usage:
# sfun_stata_round(2.5)    # Expected: 3
# sfun_stata_round(-2.5)   # Expected: -3
# sfun_stata_round(2.34, 0.1) # Expected: 2.3
# sfun_stata_round(2.35, 0.1) # Expected: 2.4
# sfun_stata_round(c(2.5, -2.5, NA, 5.25), 0.5) # Expected: c(2.5, -2.5, NA, 5.5) # Stata: round(5.25, .5) is 5.5. My formula: sign(5.25)*floor(abs(5.25)/.5 + .5)*.5 = 1*floor(10.5+0.5)*.5 = 1*floor(11)*.5 = 1*11*.5 = 5.5


