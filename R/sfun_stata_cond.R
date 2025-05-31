# Custom R implementation for Stata's cond() function.
# Stata: cond(x, a, b) returns a if x is true (non-zero and non-missing), otherwise returns b.

sfun_stata_cond = function(condition_val, true_val, false_val) {
  restore.point("sfun_stata_cond")
  # Convert condition_val to numeric. NAs in original numeric_cond will propagate.
  numeric_cond = as.numeric(condition_val)
  
  # The condition for 'true' in Stata's cond() is: not missing AND not zero.
  # This correctly handles Stata's missing values (e.g., .) and 0 as 'false'.
  is_true_in_stata = !is.na(numeric_cond) & numeric_cond != 0
  
  # Determine target type based on true_val and false_val
  # Stata's cond() type promotion: string > numeric (float > long > int > byte)
  # If either is string, result is string.
  # If both are numeric, result is numeric (float if any float, else integer).
  if (is.character(true_val) || is.character(false_val)) {
    true_val_coerced = as.character(true_val)
    false_val_coerced = as.character(false_val)
    # Stata's cond() converts numeric missing to "" if result is string.
    true_val_coerced[is.na(true_val_coerced)] = ""
    false_val_coerced[is.na(false_val_coerced)] = ""
    return(dplyr::if_else(is_true_in_stata, true_val_coerced, false_val_coerced))
  } else {
    # If not character, both are numeric or convertible to numeric.
    # To avoid integer/double mismatch with NA_real_ (which is double),
    # ensure both `true_val` and `false_val` are converted to the most general numeric type (double)
    # before `if_else`. The final casting to integer/double will be handled in t_generate/t_replace
    # based on Stata's type rules for the target variable.
    return(dplyr::if_else(is_true_in_stata, as.numeric(true_val), as.numeric(false_val)))
  }
}


