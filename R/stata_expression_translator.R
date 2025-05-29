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
  if (is.na(stata_expr) || stata_expr == "") {
    # If the expression is empty or NA, it generally means missing.
    # For Stata, missing numeric is '.', missing string is "".
    # In R context for translation, if an expression is missing, it should resolve to NA.
    # The type of NA (numeric, logical, character) depends on the context.
    # Since this function is general, assume numeric NA_real_ as default for missing expressions.
    # If this is used as a logical condition (e.g., in an 'if' clause),
    # `dplyr::coalesce(NA_real_, FALSE)` will correctly yield FALSE.
    return("NA_real_") 
  }

  r_expr = as.character(stata_expr) # Ensure r_expr is always a character string

  # Step 1: Handle Stata missing value literals '.', '.a', ..., '.z'
  # This must happen early to prevent interference with decimal numbers or variable names.
  # Regex matches '.' or '.a' to '.z' only when not part of a number or a variable name.
  r_expr = stringi::stri_replace_all_regex(r_expr, "(?<![0-9a-zA-Z_])\\.[a-zA-Z]?(?![0-9a-zA-Z_])", "NA_real_")

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
  # Stata `X == .` -> R `sfun_missing(X)`
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\b[a-zA-Z_][a-zA-Z0-9_.]*\\b)\\s*==\\s*NA_real_", "sfun_missing($1)")
  # Stata `X != .` -> R `!sfun_missing($1)`
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\b[a-zA-Z_][a-zA-Z0-9_.]*\\b)\\s*!=\\s*NA_real_", "!sfun_missing($1)")

  r_expr = stringi::stri_replace_all_regex(r_expr, "(?<![<>!=~])\\s*=\\s*(?![=])", " == ") # Replace single = with == if not part of other ops
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\s+~=\\s+", " != ") # Stata `~=` to R `!=`


  # Step 4: Translate Stata special variables and indexing (e.g., _n, _N, var[_n-1])
  # These are generally fixed references, not nested functions.
  # Use dplyr::lag/lead which are context-aware in grouped operations.

  # Directly replace with `n = $2`
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\w+)\\[_n\\s*-\\s*(\\d+)\\]", "dplyr::lag(`$1`, n = $2)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\w+)\\[_n\\s*\\+\\s*(\\d+)\\]", "dplyr::lead(`$1`, n = $2)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\w+)\\[_n\\]", "`$1`") # Backtick for consistency, though not strictly needed here

  # Handle _n and _N.
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\b_n\\b", "dplyr::row_number()")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\b_N\\b", "dplyr::n()")


  # Step 5: Iteratively translate Stata functions (e.g., cond(), round(), log(), etc.)
  # This loop handles nested function calls by repeatedly applying transformations.
  old_r_expr = ""
  # Ensure the loop condition always evaluates to a concrete TRUE/FALSE
  while (dplyr::coalesce(r_expr != old_r_expr, FALSE)) {
    old_r_expr = r_expr

    # Apply more specific regexes first if there are overlaps (e.g., round(x,y) before round(x))
    # FIX: For cond(), ensure the condition ($1) is converted to numeric before as.logical,
    # and coalesce with 0 to handle Stata's missing-as-false logic.
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bcond\\(([^,]+),([^,]+),([^)]+)\\)", "dplyr::if_else(as.logical(dplyr::coalesce(as.numeric($1), 0)), $2, $3)")
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
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bruniform\\(\\)", "stats::runif(dplyr::n())") # Stata runiform()
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

  # Defensive check: if r_expr became empty or NA for some reason after function translations.
  # This check is moved here to ensure subsequent string manipulations (backticking, + operator) don't fail.
  if (is.na(r_expr) || r_expr == "") {
      warning(paste0("R expression became NA or empty after function translation. Original Stata expression: '", stata_expr, "'"))
      return("NA_real_") 
  }

  # Step 6: Quote bare variable names with backticks
  # Define R keywords and literals that should not be backticked.
  r_reserved_words = c(
    "TRUE", "FALSE", "NA_real_", "NA_character_", "NA_integer_", "NA", "NULL",
    "if_else", "coalesce", "row_number", "n", "lag", "lead", "select", "filter",
    "mutate", "group_by", "ungroup", "syms", "all_of", "everything", "matches",
    "pivot_wider", "pivot_longer", "read_dta", "write_dta", "labelled", "zap_labels",
    "zap_formats", "zap_missing", "as_factor", "parse_number",
    "mean", "sum", "median", "sd", "min", "max", "log", "sqrt", "trunc", "rank",
    "rowSums", "rowMeans", "setNames", "match", "tempfile", "file.path",
    "fmean", "fsum", "fN", "ffirst", "flast", "fmin", "fmax", "fmedian", "fsd",
    "fquantile", "fgroup_by", "fungroup", "fsubset", "frename", "bind_rows", "rep",
    "as_tibble", "inherits", "format", "as.Date", "as.numeric", "as.character", "as.integer",
    "sign", "floor", "abs", "pmax", "stringi", "base", "stats", "dplyr", "collapse", "haven",
    "readr", "tidyr", "labelled", "restorepoint", "stata2r_env",
    "sfun_missing", "sfun_stata_add", "sfun_stata_round", "sfun_string", "sfun_stritrim",
    "sfun_strpos", "sfun_subinstr", "sfun_stata_mdy", "sfun_stata_date", "sfun_day",
    "sfun_month", "sfun_qofd", "sfun_dow", "sfun_normalize_string_nas", "sfun_strip_stata_attributes",
    "sfun_compress_col_type", "sfun_is_stata_expression_string_typed", "as.logical"
  )
  
  # Find all occurrences of words matching Stata variable names and their locations
  locations = stringi::stri_locate_all_regex(r_expr, "\\b([a-zA-Z_][a-zA-Z0-9_.]*)\\b")[[1]]
  
  if (!is.null(locations) && NROW(locations) > 0) {
      # Sort locations by end position descending to avoid issues with replacement as `r_expr` changes length
      locations = locations[order(locations[,2], decreasing = TRUE), , drop = FALSE]
      
      for (k in seq_len(NROW(locations))) {
          start_pos = locations[k,1]
          end_pos = locations[k,2]
          current_word = stringi::stri_sub(r_expr, start_pos, end_pos)
          
          # Only backtick if it's not an R reserved word, not a numeric literal, and not already backticked
          is_reserved = dplyr::coalesce(current_word %in% r_reserved_words, FALSE)
          is_numeric_literal = dplyr::coalesce(suppressWarnings(!is.na(as.numeric(current_word))), FALSE)
          
          # Check if already backticked (by checking characters around it)
          is_already_backticked = FALSE
          # Ensure indices are valid before accessing substrings
          # FIX: Make this if condition robust to NA results from stringi::stri_length(r_expr)
          # The fix for char_before/char_after NA values is to coalesce them.
          if (dplyr::coalesce(start_pos > 1 && end_pos <= stringi::stri_length(r_expr), FALSE)) { 
            char_before = dplyr::coalesce(stringi::stri_sub(r_expr, start_pos - 1, start_pos - 1), "") # Coalesce NA to ""
            char_after = dplyr::coalesce(stringi::stri_sub(r_expr, end_pos + 1, end_pos + 1), "") # Coalesce NA to ""
            # Ensure char_before and char_after are not NA before comparison
            # Coalesce ensures logical comparison even if stri_sub returns NA for out-of-bounds access (though guarded by if)
            is_already_backticked = (char_before == "`" && char_after == "`")
          }
          
          # This is the line that caused the error. Coalesce each part to FALSE if NA.
          # The `dplyr::coalesce` calls on `is_reserved`, `is_numeric_literal`, `is_already_backticked`
          # already ensure they are not NA. The error might be a subtle bug in R's `&&` operator
          # with some specific `NA` values or the underlying C code.
          # Explicitly ensuring they are single logicals here might prevent it.
          local_is_reserved = as.logical(dplyr::coalesce(is_reserved, FALSE))
          local_is_numeric_literal = as.logical(dplyr::coalesce(is_numeric_literal, FALSE))
          local_is_already_backticked = as.logical(dplyr::coalesce(is_already_backticked, FALSE))

          # FIX: Apply isTRUE() to each logical operand to ensure the `if` condition never evaluates to NA.
          if (isTRUE(!local_is_reserved) && isTRUE(!local_is_numeric_literal) && isTRUE(!local_is_already_backticked)) {
              # Replace the word with its backticked version
              r_expr = paste0(stringi::stri_sub(r_expr, 1, start_pos - 1),
                              "`", current_word, "`",
                              stringi::stri_sub(r_expr, end_pos + 1, stringi::stri_length(r_expr)))
          }
      }
  }


  # Step 7: Translate Stata '+' operator to sfun_stata_add for polymorphic behavior
  # This must happen after all functions and variable names are translated/backticked.
  # Operands can now be: quoted strings, numbers, R literals, backticked variable names, or R function calls.
  
  # Updated operand_pattern to include backticked variable names
  operand_pattern = "(?:\"[^\"]*\"|'[^']*'|\\d+(?:\\.\\d+)?|\\b(?:NA_real_|NULL)\\b|\\b(?:TRUE|FALSE)\\b|`[^`]+`|\\b[a-zA-Z_][a-zA-Z0-9_:]*\\s*\\(.*?\\)\\s*)"

  old_r_expr_add = ""
  while (dplyr::coalesce(r_expr != old_r_expr_add, FALSE)) {
    old_r_expr_add = r_expr
    # Regex: Match 'operand' + 'operand', ensuring `+` is an operator not part of `==` etc.
    # The `(?<![<>=!~])` ensures `+` is not preceded by <, >, =, !, ~.
    # The `(?!\\s*\\+|\\s*=\\s*)` ensures `+` is not followed by another `+` or `=` (for `++` or `+=`).
    add_regex_middle_part = "\\s*(?<![<>=!~])\\+\\s*(?!\\s*\\+|\\s*=\\s*)"
    add_regex_full = paste0("(", operand_pattern, ")", add_regex_middle_part, "(", operand_pattern, ")")
    r_expr = stringi::stri_replace_all_regex(r_expr, add_regex_full, "sfun_stata_add($1, $2)")
  }

  return(r_expr)
}


