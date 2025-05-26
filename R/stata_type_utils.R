# Helper function to determine if a Stata expression is likely to produce a string result in R.
is_stata_expr_string_type = function(stata_expr) {
  if (is.na(stata_expr)) return(FALSE)
  # Check for common Stata string functions
  # Note: `string()` converts numeric to string, so it implies a string output.
  string_funcs = c("strtrim", "stritrim", "lower", "upper", "substr", "subinstr", "strpos", "length", "strlen", "string")
  if (any(sapply(string_funcs, function(f) stringi::stri_detect_fixed(stata_expr, paste0(f, "("))))) {
    return(TRUE)
  }
  # Check for string literals (quoted strings)
  if (stringi::stri_startswith_fixed(stata_expr, '"') && stringi::stri_endswith_fixed(stata_expr, '"')) {
    return(TRUE)
  }
  if (stringi::stri_startswith_fixed(stata_expr, "'") && stringi::stri_endswith_fixed(stata_expr, "'")) {
    return(TRUE)
  }
  FALSE
}

