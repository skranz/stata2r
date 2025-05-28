# Custom R implementation for Stata's string() behavior
# Stata: string(x) converts numeric x to string.
# Missing numeric values (.) are converted to an empty string ("").

sfun_string = function(x) {
  restore.point("sfun_string")
  
  # Initialize result vector of character type
  res = character(length(x))
  
  # Handle non-missing values: convert them to character
  # This correctly converts numeric values to their string representation.
  res[!is.na(x)] = as.character(x[!is.na(x)])
  
  # Handle missing values (NA in R): convert them to an empty string ("")
  # This directly addresses the Stata `.` to `""` conversion rule.
  res[is.na(x)] = ""
  
  return(res)
}

