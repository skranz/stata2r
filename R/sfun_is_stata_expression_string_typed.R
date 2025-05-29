is_stata_expression_string_typed = function(stata_expr_original) {
  restore.point("is_stata_expression_string_typed")
  
  if (is.na(stata_expr_original) || stata_expr_original == "") return(FALSE)

  # 1. Contains any string literal (text enclosed in double or single quotes)
  if (stringi::stri_detect_regex(stata_expr_original, '"[^"]*"|\'[^\']\'')) {
    return(TRUE)
  }
  
  # 2. Contains Stata functions that produce a string type
  # This list focuses on functions whose output type is always string.
  string_producing_functions = c(
    "char", "itrim", "lower", "ltrim", "proper", "rtrim", "string", "subinstr",
    "substr", "strl", "strpos", "strreverse", "strtrim", "trim", "upper",
    "ustrleft", "ustrlower", "ustrpos", "ustrright", "ustrtrim", "ustrunescape",
    "ustrupper", "ustrword", "ustrwordcount", "word", "wordcount"
  )
  for (func in string_producing_functions) {
    # Match function calls like func(...) but not variable names that contain func.
    # Use word boundary `\b` to avoid partial matches.
    # Also handle possible spaces after function name: `func (arg)`
    if (stringi::stri_detect_regex(stata_expr_original, paste0("\\b", func, "\\s*\\("))) {
      return(TRUE)
    }
  }
  
  # 3. `cond(condition, val_if_true, val_if_false)`: if any value (val_if_true, val_if_false) is string, result is string.
  # This is complex to parse perfectly with simple regex, but a basic check can be added if needed.
  # For now, relying on explicit string literals or string functions within cond's arguments is assumed.
  
  return(FALSE)
}

