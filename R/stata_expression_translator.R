# Stata Expression Translator

# Placeholder for Stata results (r() and e() values)
# This environment would be populated by commands like summarize, regress
# For now, we will generate unique variable names for r() values.
# stata_results_env = new.env(parent = emptyenv())


# Translates a Stata expression string into an R expression string
# stata_expr: The Stata expression (e.g., "var1 + log(var2)")
# context: Information about the context (e.g., if inside a by group, for _n/_N)
# r_value_mappings: A named list/vector mapping Stata r-value names (e.g. "r(mean)")
#                   to R variable names (e.g. "stata_r_val_L5_mean")
translate_stata_expression_to_r = function(stata_expr, context = list(is_by_group = FALSE), r_value_mappings = NULL) {
  restore.point("translate_stata_expression_to_r")
  if (is.na(stata_expr) || stata_expr == "") return(NA_character_)

  r_expr = stata_expr

  # Step 1: Handle Stata missing value literal '.' (MOVED AND REFINED)
  # This must happen early to prevent interference with decimal numbers.
  # Use negative lookbehind/lookahead to match a dot ONLY if it's not part of a number.
  r_expr = stringi::stri_replace_all_regex(r_expr, "(?<![0-9])\\.(?![0-9])", "NA_real_")

  # Step 2: Handle r() values using the mapping.
  # This ensures that r() values are replaced by their corresponding R variable names
  # BEFORE other transformations (like missing value checks) are applied to them.
  if (!is.null(r_value_mappings) && length(r_value_mappings) > 0) {
    for (stata_r_name in names(r_value_mappings)) {
      # Escape parentheses for regex matching
      stata_r_regex = gsub("(", "\\(", gsub(")", "\\)", stata_r_name, fixed=TRUE), fixed=TRUE)
      r_expr = stringi::stri_replace_all_regex(r_expr, stata_r_regex, r_value_mappings[[stata_r_name]])
    }
  }

  # Step 3: Translate Stata logical operators and missing value comparisons.
  # These must happen after handling `r()` values so `r(mean)` is already `stata_r_val_Lxx_mean`.
  # Stata `X == .` -> R `is.na(X)`
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\b[a-zA-Z_][a-zA-Z0-9_.]*\\b)\\s*==\\s*NA_real_", "is.na($1)")
  # Stata `X != .` -> R `!is.na($1)`
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\b[a-zA-Z_][a-zA-Z0-9_.]*\\b)\\s*!=\\s*NA_real_", "!is.na($1)") # Use stri_replace_all_regex for safety

  r_expr = stringi::stri_replace_all_regex(r_expr, "(?<![<>!=~])\\s*=\\s*(?![=])", " == ") # Replace single = with == if not part of other ops
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\s+~=\\s+", " != ") # Stata `~=` to R `!=`


  # Step 4: Translate Stata special variables and indexing (e.g., _n, _N, var[_n-1])
  # These are generally fixed references, not nested functions.
  # Use dplyr::lag/lead which are context-aware in grouped operations.

  # Directly replace with `n = $2`
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\w+)\\[_n\\s*-\\s*(\\d+)\\]", "dplyr::lag($1, n = $2)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\w+)\\[_n\\s*\\+\\s*(\\d+)\\]", "dplyr::lead($1, n = $2)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\w+)\\[_n\\]", "$1")

  # Handle _n and _N.
  # Convert to numeric to match Stata's default float storage for integers.
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\b_n\\b", "as.numeric(dplyr::row_number())")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\b_N\\b", "as.numeric(dplyr::n())")


  # Step 5: Iteratively translate Stata functions (e.g., cond(), round(), log(), etc.)
  # This loop handles nested function calls by repeatedly applying transformations.
  old_r_expr = ""
  while (r_expr != old_r_expr) {
    old_r_expr = r_expr

    # Apply more specific regexes first if there are overlaps (e.g., round(x,y) before round(x))
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bcond\\(([^,]+),([^,]+),([^)]+)\\)", "dplyr::if_else($1, $2, $3)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bround\\(([^,]+),([^)]+)\\)", "sfun_stata_round($1, $2)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bround\\(([^)]+)\\)", "sfun_stata_round($1, 1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bmod\\(([^,]+),([^)]+)\\)", "($1 %% $2)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bmissing\\(([^)]+)\\)", "sfun_missing($1)") # Changed to sfun_missing
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\blog\\(([^)]+)\\)", "log($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bsqrt\\(([^)]+)\\)", "sqrt($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bint\\(([^)]+)\\)", "trunc($1)") # Stata int() truncates

    # String functions (using stringi or custom helpers)
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstrtrim\\(([^)]+)\\)", "stringi::stri_trim_right($1)") # Corrected for Stata's strtrim
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstritrim\\(([^)]+)\\)", "sfun_stritrim($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\blower\\(([^)]+)\\)", "stringi::stri_trans_tolower($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bupper\\(([^)]+)\\)", "stringi::stri_trans_toupper($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bsubstr\\(([^,]+),([^,]+),([^)]+)\\)", "stringi::stri_sub($1, from = $2, length = $3)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bsubinstr\\(([^,]+),([^,]+),([^,]+),([^)]+)\\)", "sfun_subinstr($1, $2, $3, $4)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstrpos\\(([^,]+),([^)]+)\\)", "sfun_strpos($1, $2)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\blength\\(([^)]+)\\)", "stringi::stri_length($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstrlen\\(([^)]+)\\)", "stringi::stri_length($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstring\\(([^)]+)\\)", "sfun_string($1)") # CHANGED HERE
    # Random number generator functions
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bruniform\\(\\)", "stats::runif(as.numeric(dplyr::n()))") # Stata runiform()
    # Date functions
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bdate\\(([^,]+),([^,]+),([^)]+)\\)", "sfun_stata_date($1, $2, $3)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bdate\\(([^,]+),([^)]+)\\)", "sfun_stata_date($1, $2)")
    # NEW: mdy() function
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bmdy\\(([^,]+),([^,]+),([^)]+)\\)", "sfun_stata_mdy($1, $2, $3)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\byear\\(([^)]+)\\)", "as.numeric(format(as.Date($1, origin = '1960-01-01'), '%Y'))") # Stata date epoch is 1960-01-01
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bmonth\\(([^)]+)\\)", "sfun_month($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bday\\(([^)]+)\\)", "sfun_day($1)") # Use sfun_day
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bqofd\\(([^)]+)\\)", "sfun_qofd($1)") # Use sfun_qofd
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bdow\\(([^)]+)\\)", "sfun_dow($1)") # NEW: Use sfun_dow
  }

  # Step 6: Translate Stata '+' operator to sfun_stata_add for polymorphic behavior
  # This needs to be applied iteratively until no more '+' signs (that are not part of comparison operators) exist.
  # The regex for operands must be robust to capture complete R expressions, including nested function calls.

  # Define a robust pattern for an R expression unit that can be an operand.
  # This pattern matches:
  # 1. A variable name (starts with letter/underscore, then alphanumeric/underscore/dot)
  # 2. A numeric literal
  # 3. A string literal (single or double quoted)
  # 4. A function call (e.g., `log(x)`, `sfun_stata_add(a, b)`)
  operand_regex = paste0(
      "(?:",
      "[a-zA-Z_][a-zA-Z0-9_.]*|", # Variable name
      "\\d+(?:\\.\\d+)?|",       # Numeric literal
      "\"[^\"]*\"|'[^']*'|",     # String literal (double or single quoted)
      "\\b\\w+\\((?:[^()]|\\b\\w+\\([^()]*\\))*\\)", # Function call (allowing one level of nested simple function calls)
      ")"
  )

  old_r_expr_add = ""
  while (r_expr != old_r_expr_add) {
    old_r_expr_add = r_expr
    # Regex: Match 'operand' + 'operand', where an operand is defined by `operand_regex`.
    # It ensures that `+` is treated as an operator, not part of `==` or `!=`.
    r_expr = stringi::stri_replace_all_regex(r_expr,
                                           paste0("(", operand_regex, ")\\s*(?<![<>=!~])\\+\\s*(?!\\s*\\+|\\s*=\\s*)(", operand_regex, ")"),
                                           "sfun_stata_add($1, $2)")
  }

  # Defensive check: if r_expr became empty or NA for some reason (should not happen for valid input)
  if (is.na(r_expr) || r_expr == "") {
      warning(paste0("R expression became NA or empty during translation. Original Stata expression: '", stata_expr, "'"))
      return(NA_character_)
  }

  return(r_expr)
}

