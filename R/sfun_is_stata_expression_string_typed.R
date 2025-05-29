sfun_is_stata_expression_string_typed = function(stata_expr_original) {
  restore.point("sfun_is_stata_expression_string_typed")
  
  # Ensure stata_expr_original is a single character string or NA
  if (is.null(stata_expr_original) || length(stata_expr_original) == 0 || !is.character(stata_expr_original)) {
      stata_expr_original = NA_character_
  } else {
      stata_expr_original = as.character(stata_expr_original[1])
  }

  if (is.na(stata_expr_original) || stata_expr_original == "") return(FALSE)

  # NEW: 1. Check for logical/comparison operators. If present, the result is numeric (0/1).
  # This must precede the string literal check.
  if (dplyr::coalesce(stringi::stri_detect_regex(stata_expr_original, "==|!=|<=|>=|<|>|&|\\|"), FALSE)) {
    return(FALSE)
  }

  # 2. `cond(condition, val_if_true, val_if_false)`: if any value (val_if_true, val_if_false) is string, result is string.
  #    This check must come BEFORE generic string literal check.
  cond_match = stringi::stri_match_first_regex(stata_expr_original, "\\bcond\\(([^,]+),([^,]+),([^)]+)\\)")
  if (!is.na(cond_match[1,1])) {
      val_if_true_str = stringi::stri_trim_both(cond_match[1,3])
      val_if_false_str = stringi::stri_trim_both(cond_match[1,4])
      # Recursively check the arguments for string type
      if (dplyr::coalesce(sfun_is_stata_expression_string_typed(val_if_true_str), FALSE) ||
          dplyr::coalesce(sfun_is_stata_expression_string_typed(val_if_false_str), FALSE)) {
          return(TRUE)
      } else {
          # If both are numeric, cond is numeric
          return(FALSE)
      }
  }

  # 3. Check for explicitly numeric-returning functions. If found, return FALSE immediately.
  numeric_producing_functions = c(
    "log", "sqrt", "int", "round", "mod", "runiform", "mdy", "date",
    "year", "month", "day", "qofd", "dow", "missing",
    # Stata type casting functions that convert to numeric:
    "float", "double", "long", "int", "byte"
  )
  for (func in numeric_producing_functions) {
    if (dplyr::coalesce(stringi::stri_detect_regex(stata_expr_original, paste0("\\b", func, "\\s*\\(")), FALSE)) {
      return(FALSE)
    }
  }

  # 4. Check for explicitly string-returning functions.
  string_producing_functions = c(
    "char", "itrim", "lower", "ltrim", "proper", "rtrim", "string", "subinstr",
    "substr", "strl", "strpos", "strreverse", "strtrim", "trim", "upper",
    "ustrleft", "ustrlower", "ustrpos", "ustrright", "ustrtrim", "ustrunescape",
    "ustrupper", "ustrword", "ustrwordcount", "word", "wordcount",
    # Stata type casting functions that convert to string:
    "string"
  )
  for (func in string_producing_functions) {
    if (dplyr::coalesce(stringi::stri_detect_regex(stata_expr_original, paste0("\\b", func, "\\s*\\(")), FALSE)) {
      return(TRUE)
    }
  }
  
  # NEW: 5. Check for '+' operator and string literals.
  #    If expression contains '+' AND a string literal, it's likely string concatenation.
  #    This check is a heuristic.
  if (dplyr::coalesce(stringi::stri_detect_fixed(stata_expr_original, "+"), FALSE) &&
      dplyr::coalesce(stringi::stri_detect_regex(stata_expr_original, '"[^"]*"|\'[^\']*\'' ), FALSE)) {
      return(TRUE)
  }

  # 6. Contains any string literal (text enclosed in double or single quotes)
  #    This must come after function/operator checks.
  if (dplyr::coalesce(stringi::stri_detect_regex(stata_expr_original, '"[^"]*"|\'[^\']*\'' ), FALSE)) {
    return(TRUE)
  }

  # If none of the above rules apply, default to numeric.
  # This implies that if it's a variable reference, it's numeric unless explicitly string.
  # Or if it's a simple arithmetic expression, it's numeric.
  return(FALSE)
}


