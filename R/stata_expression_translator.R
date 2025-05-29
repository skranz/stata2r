translate_stata_expression_to_r = function(stata_expr, context = list(is_by_group = FALSE), r_value_mappings = NULL) {
  restore.point("translate_stata_expression_to_r")
  
  # Ensure stata_expr is a single character string or NA
  if (is.null(stata_expr) || length(stata_expr) == 0) {
      stata_expr = NA_character_
  } else {
      stata_expr = as.character(stata_expr[1]) # Ensure it's a single string
  }

  if (is.na(stata_expr) || stata_expr == "") {
    return("NA_real_") 
  }

  r_expr = stata_expr

  # --- New: Handle string literals by replacing them with unique placeholders ---
  # This prevents them from being backticked or otherwise mangled by other regexes.
  string_literal_map = list()
  # Find all string literals (double or single quoted)
  literal_matches = stringi::stri_match_all_regex(r_expr, '"[^"]*"|\'[^\']*\'')[[1]]
  
  if (!is.null(literal_matches) && NROW(literal_matches) > 0) {
      # Iterate and replace. Sorting by end position descending is safer for overlapping regexes
      # but here we are replacing fixed strings, so order doesn't strictly matter.
      # However, to avoid issues if a placeholder becomes part of a later literal,
      # it's good to replace in a way that doesn't affect other parts of the string being processed.
      # For this simple replacement, a simple loop over matches is fine.
      
      # Use a unique counter for placeholders
      placeholder_counter = 0
      for (k in seq_len(NROW(literal_matches))) {
          literal_text = literal_matches[k, 1]
          # Check if this literal_text is already a placeholder from a previous iteration (unlikely but safe)
          if (stringi::stri_startswith_fixed(literal_text, "STATA2R_STR_LITERAL_PLACEHOLDER_") && stringi::stri_endswith_fixed(literal_text, "_")) {
            next # Already a placeholder, skip
          }
          placeholder_counter = placeholder_counter + 1
          placeholder = paste0("STATA2R_STR_LITERAL_PLACEHOLDER_", placeholder_counter, "_")
          string_literal_map[[placeholder]] = literal_text
          # Replace only the first occurrence for this literal to handle duplicates correctly in a loop
          r_expr = stringi::stri_replace_first_fixed(r_expr, literal_text, placeholder)
      }
  }
  # --- End new string literal handling ---


  # Step 1: Handle Stata missing value literals '.', '.a', ..., '.z'
  r_expr = stringi::stri_replace_all_regex(r_expr, "(?<![0-9a-zA-Z_])\\.[a-zA-Z]?(?![0-9a-zA-Z_])", "NA_real_")

  # Step 2: Handle r() values using the mapping.
  if (!is.null(r_value_mappings) && length(r_value_mappings) > 0) {
    for (stata_r_name in names(r_value_mappings)) {
      stata_r_regex = gsub("(", "\\(", gsub(")", "\\)", stata_r_name, fixed=TRUE), fixed=TRUE)
      r_expr = stringi::stri_replace_all_regex(r_expr, stata_r_regex, r_value_mappings[[stata_r_name]])
    }
  }

  # Step 3: Translate Stata logical operators and missing value comparisons.
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\b[a-zA-Z_][a-zA-Z0-9_.]*\\b)\\s*==\\s*NA_real_", "sfun_missing($1)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\b[a-zA-Z_][a-zA-Z0-9_.]*\\b)\\s*!=\\s*NA_real_", "!sfun_missing($1)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "(?<![<>=!~])\\s*=\\s*(?![=])", " == ")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\s+~=\\s+", " != ")


  # Step 4: Translate Stata special variables and indexing (e.g., _n, _N, var[_n-1])
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\w+)\\[_n\\s*-\\s*(\\d+)\\]", "dplyr::lag(`$1`, n = $2)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\w+)\\[_n\\s*\\+\\s*(\\d+)\\]", "dplyr::lead(`$1`, n = $2)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\w+)\\[_n\\]", "`$1`")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\b_n\\b", "dplyr::row_number()")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\b_N\\b", "dplyr::n()")


  # Step 5: Iteratively translate Stata functions (e.g., cond(), round(), log(), etc.)
  old_r_expr = ""
  while (dplyr::coalesce(r_expr != old_r_expr, FALSE)) {
    old_r_expr = r_expr

    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bcond\\(([^,]+),([^,]+),([^)]+)\\)", "dplyr::if_else(as.logical(dplyr::coalesce(as.numeric($1), 0)), $2, $3)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bround\\(([^,]+),([^)]+)\\)", "sfun_stata_round($1, $2)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bround\\(([^)]+)\\)", "sfun_stata_round($1, 1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bmod\\(([^,]+),([^)]+)\\)", "($1 %% $2)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bmissing\\(([^)]+)\\)", "sfun_missing($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\blog\\(([^)]+)\\)", "log($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bsqrt\\(([^)]+)\\)", "sqrt($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bint\\(([^)]+)\\)", "trunc($1)")

    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstrtrim\\(([^)]+)\\)", "stringi::stri_trim_right($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstritrim\\(([^)]+)\\)", "sfun_stritrim($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\blower\\(([^)]+)\\)", "stringi::stri_trans_tolower($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bupper\\(([^)]+)\\)", "stringi::stri_trans_toupper($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bsubstr\\(([^,]+),([^,]+),([^)]+)\\)", "stringi::stri_sub($1, from = $2, length = $3)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bsubinstr\\(([^,]+),([^,]+),([^,]+),([^)]+)\\)", "sfun_subinstr($1, $2, $3, $4)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstrpos\\(([^,]+),([^)]+)\\)", "sfun_strpos($1, $2)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\blength\\(([^)]+)\\)", "stringi::stri_length($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstrlen\\(([^)]+)\\)", "stringi::stri_length($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstring\\(([^)]+)\\)", "sfun_string($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bruniform\\(\\)", "stats::runif(dplyr::n())")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bdate\\(([^,]+),([^,]+),([^)]+)\\)", "sfun_stata_date($1, $2, $3)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bdate\\(([^,]+),([^)]+)\\)", "sfun_stata_date($1, $2)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bmdy\\(([^,]+),([^,]+),([^)]+)\\)", "sfun_stata_mdy($1, $2, $3)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\byear\\(([^)]+)\\)", "as.numeric(format(as.Date($1, origin = '1960-01-01'), '%Y'))")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bmonth\\(([^)]+)\\)", "sfun_month($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bday\\(([^)]+)\\)", "sfun_day($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bqofd\\(([^)]+)\\)", "sfun_qofd($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bdow\\(([^)]+)\\)", "sfun_dow($1)")
  }

  if (is.na(r_expr) || r_expr == "") {
      warning(paste0("R expression became NA or empty after function translation. Original Stata expression: '", stata_expr, "'"))
      return("NA_real_") 
  }

  # Step 6: Quote bare variable names with backticks
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
  
  locations = stringi::stri_locate_all_regex(r_expr, "\\b([a-zA-Z_][a-zA-Z0-9_.]*)\\b")[[1]]
  
  if (!is.null(locations) && NROW(locations) > 0) {
      locations = locations[order(locations[,2], decreasing = TRUE), , drop = FALSE]
      
      for (k in seq_len(NROW(locations))) {
          start_pos = locations[k,1]
          end_pos = locations[k,2]
          current_word = stringi::stri_sub(r_expr, start_pos, end_pos)
          
          is_reserved = dplyr::coalesce(current_word %in% r_reserved_words, FALSE)
          is_numeric_literal = dplyr::coalesce(suppressWarnings(!is.na(as.numeric(current_word))), FALSE)
          
          is_already_backticked = FALSE
          if (dplyr::coalesce(start_pos > 1 && end_pos <= stringi::stri_length(r_expr), FALSE)) { 
            char_before = dplyr::coalesce(stringi::stri_sub(r_expr, start_pos - 1, start_pos - 1), "")
            char_after = dplyr::coalesce(stringi::stri_sub(r_expr, end_pos + 1, end_pos + 1), "")
            is_already_backticked = (char_before == "`" && char_after == "`")
          }
          
          local_is_reserved = as.logical(dplyr::coalesce(is_reserved, FALSE))
          local_is_numeric_literal = as.logical(dplyr::coalesce(is_numeric_literal, FALSE))
          local_is_already_backticked = as.logical(dplyr::coalesce(is_already_backticked, FALSE))

          if (isTRUE(!local_is_reserved) && isTRUE(!local_is_numeric_literal) && isTRUE(!local_is_already_backticked)) {
              r_expr = paste0(stringi::stri_sub(r_expr, 1, start_pos - 1),
                              "`", current_word, "`",
                              stringi::stri_sub(r_expr, end_pos + 1, stringi::stri_length(r_expr)))
          }
      }
  }


  # Step 7: Translate Stata '+' operator to sfun_stata_add for polymorphic behavior
  operand_pattern = "(?:\"[^\"]*\"|'[^']*'|\\d+(?:\\.\\d+)?|\\b(?:NA_real_|NULL)\\b|\\b(?:TRUE|FALSE)\\b|`[^`]+`|\\b[a-zA-Z_][a-zA-Z0-9_:]*\\s*\\(.*?\\)\\s*)"

  old_r_expr_add = ""
  while (dplyr::coalesce(r_expr != old_r_expr_add, FALSE)) {
    old_r_expr_add = r_expr
    add_regex_middle_part = "\\s*(?<![<>=!~])\\+\\s*(?!\\s*\\+|\\s*=\\s*)"
    add_regex_full = paste0("(", operand_pattern, ")", add_regex_middle_part, "(", operand_pattern, ")")
    r_expr = stringi::stri_replace_all_regex(r_expr, add_regex_full, "sfun_stata_add($1, $2)")
  }

  # --- New: Restore string literals from placeholders ---
  # Iterate in reverse order of placeholder creation to handle potential nested replacements
  # though for string literals, simple fixed replacement is usually fine.
  # Sorting by the length of the placeholder name (descending) ensures longer placeholders are replaced first,
  # preventing partial matches if placeholders were substrings of each other.
  # Here, placeholder names are unique and fixed length prefix, so simple iteration is fine.
  if (length(string_literal_map) > 0) {
      # Use a loop over sorted names to ensure deterministic order if needed, but for fixed string replacement, it's not critical.
      # For robustness, we could sort by length then alphabetically.
      sorted_placeholders = names(string_literal_map)[order(stringi::stri_length(names(string_literal_map)), names(string_literal_map), decreasing = TRUE)]
      for (placeholder in sorted_placeholders) {
          r_expr = stringi::stri_replace_all_fixed(r_expr, placeholder, string_literal_map[[placeholder]])
      }
  }
  # --- End new string literal restoration ---

  return(r_expr)
}

