# Custom R implementation for Stata's cond() function.
# Stata: cond(x, a, b) returns a if x is true (non-zero and non-missing), otherwise returns b.

sfun_stata_cond = function(condition_val, true_val, false_val) {
  restore.point("sfun_stata_cond")
  # Convert condition_val to numeric. NAs in original numeric_cond will propagate.
  numeric_cond = as.numeric(condition_val)
  
  # The condition for 'true' in Stata's cond() is: not missing AND not zero.
  # This correctly handles Stata's missing values (e.g., .) and 0 as 'false'.
  is_true_in_stata = !is.na(numeric_cond) & numeric_cond != 0
  
  # Use dplyr::if_else for vectorized conditional selection.
  # dplyr::if_else handles NA in its `condition` argument by propagating it,
  # but our `is_true_in_stata` explicitly converts NAs from `numeric_cond` to `FALSE`,
  # which matches Stata's behavior of returning `false_val` when the condition is missing.
  return(dplyr::if_else(is_true_in_stata, true_val, false_val))
}


