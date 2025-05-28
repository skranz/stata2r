is_stata_expr_string_type = function(stata_expr) {
  restore.point("is_stata_expr_string_type")
  if (is.na(stata_expr)) return(FALSE)

  # Check for string literals (quoted strings)
  # Updated regex to correctly detect both double and single quoted strings
  if (stringi::stri_detect_regex(stata_expr, '"[^"]*"|\'[^\']*\'')) {
    return(TRUE)
  }

  # Check for Stata functions that *return* string values
  # Functions that take string input and return string output:
  # strpos(), length(), strlen() return numeric values, so they are excluded from this list.
  string_output_funcs = c("strtrim", "stritrim", "lower", "upper", "substr", "subinstr")
  if (any(sapply(string_output_funcs, function(f) stringi::stri_detect_fixed(stata_expr, paste0(f, "("))))) {
    return(TRUE)
  }
  
  # The `string()` function explicitly converts a numeric value to a string.
  if (stringi::stri_detect_fixed(stata_expr, "string(")) {
      return(TRUE)
  }

  # Check for the polymorphic '+' operator.
  # If a '+' is present, and the expression contains any string literal, it implies string concatenation.
  # This is a heuristic as variable types are not known at translation time.
  # If no string literal is present, it's assumed to be numeric addition.
  if (stringi::stri_detect_fixed(stata_expr, "+") && 
      stringi::stri_detect_regex(stata_expr, '"[^"]*"|\'[^\']*\'')) {
      return(TRUE)
  }

  FALSE
}

