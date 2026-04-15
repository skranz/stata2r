An analysis of the test error shows two issues compounding each other:

1. **Improper translation of Stata's `~=` and `~` operators:** In Stata, `~=` means "not equal" and is an alias for `!=`, while `~` acts as a logical NOT (equivalent to `!`). The `translate_stata_expression_to_r` function failed to translate `~=` to `!=` when there were no surrounding spaces (like in `corrupt~=.`). Because of this, the translated R code yielded `` `corrupt`~=NA_real_ ``, which is an invalid operator in R, triggering the parsing error.
2. **Missing Evaluation of Stata Variable Abbreviations within `dplyr::mutate`:** Stata allows column names to be abbreviated (e.g. `partner_report_ex` for `partner_report_export`). While `s2r_eval_cond` correctly bypassed this by looking up abbreviations and injecting them into its custom evaluation list, `dplyr::mutate` implementations in `scmd_generate`, `scmd_replace`, etc., don't know about Stata abbreviations causing evaluating errors like `object 'partner_report_ex' not found`.

Below are the modifications to correctly handle both the `~=` operator and dynamically expand variable abbreviations inside parsed R expressions.

!MODIFICATION resolve_abbrevs in varlist.R
scope = "function"
file = "R/varlist.R"
insert_bottom = true
description = "Add helper to expand abbreviations in R expressions at runtime"
---
```r
#' Expand Stata variable abbreviations inside a translated R expression
#' @param r_expr Translated R expression string
#' @param data_cols Character vector of available columns in the dataset
#' @return R expression string with abbreviations replaced by full names
resolve_abbrevs_in_expr = function(r_expr, data_cols) {
  if (is.na(r_expr) || r_expr == "") return(r_expr)
  expr = try(base::parse(text = r_expr), silent = TRUE)
  if (inherits(expr, "try-error")) return(r_expr)
  
  used_vars = all.vars(expr)
  for (v in used_vars) {
    if (!(v %in% data_cols)) {
      mcols = data_cols[startsWith(data_cols, v)]
      if (length(mcols) == 1) {
        # Escape v for regex
        v_escaped = stringi::stri_replace_all_regex(v, "([.])", "\\\\$1")
        regex = paste0("(?<![a-zA-Z0-9_\\.])`?", v_escaped, "`?(?![a-zA-Z0-9_\\.])")
        r_expr = stringi::stri_replace_all_regex(r_expr, regex, paste0("`", mcols[1], "`"))
      }
    }
  }
  return(r_expr)
}
```
!END_MODIFICATION resolve_abbrevs in varlist.R

!MODIFICATION sfun_is_stata_expression_string_typed sfun_is_stata_expression_string_typed.R
scope = "function"
file = "R/sfun_is_stata_expression_string_typed.R"
function_name = "sfun_is_stata_expression_string_typed"
description = "Add ~= to the list of logical operators"
---
```r
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
  if (fast_coalesce(stringi::stri_detect_regex(stata_expr_original, "==|!=|~=|<=|>=|<|>|&|\\|"), FALSE)) {
    return(FALSE)
  }

  # 2. `cond(condition, val_if_true, val_if_false)`: if any value (val_if_true, val_if_false) is string, result is string.
  #    This check must come BEFORE generic string literal check.
  cond_match = stringi::stri_match_first_regex(stata_expr_original, "\\bcond\\(([^,]+),([^,]+),([^)]+)\\)")
  if (!is.na(cond_match[1,1])) {
      val_if_true_str = stringi::stri_trim_both(cond_match[1,3])
      val_if_false_str = stringi::stri_trim_both(cond_match[1,4])
      # Recursively check the arguments for string type
      if (fast_coalesce(sfun_is_stata_expression_string_typed(val_if_true_str), FALSE) ||
          fast_coalesce(sfun_is_stata_expression_string_typed(val_if_false_str), FALSE)) {
          return(TRUE)
      } else {
          # If both are numeric, cond is numeric
          return(FALSE)
      }
  }

  # 3. Check for explicitly numeric-returning functions. If found, return FALSE immediately.
  numeric_producing_functions = c(
    "log", "sqrt", "int", "round", "mod", "runiform", "mdy", "date",
    "year", "month", "day", "qofd", "dow", "missing", "inlist", "inrange",
    # Stata type casting functions that convert to numeric:
    "float", "double", "long", "int", "byte"
  )
  for (func in numeric_producing_functions) {
    if (fast_coalesce(stringi::stri_detect_regex(stata_expr_original, paste0("\\b", func, "\\s*\\(")), FALSE)) {
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
    if (fast_coalesce(stringi::stri_detect_regex(stata_expr_original, paste0("\\b", func, "\\s*\\(")), FALSE)) {
      return(TRUE)
    }
  }

  # NEW: 5. Check for '+' operator and string literals.
  #    If expression contains '+' AND a string literal, it's likely string concatenation.
  #    This check is a heuristic.
  if (fast_coalesce(stringi::stri_detect_fixed(stata_expr_original, "+"), FALSE) &&
      fast_coalesce(stringi::stri_detect_regex(stata_expr_original, '"[^"]*"|\'[^\']*\'' ), FALSE)) {
      return(TRUE)
  }

  # 6. Contains any string literal (text enclosed in double or single quotes)
  #    This must come after function/operator checks.
  if (fast_coalesce(stringi::stri_detect_regex(stata_expr_original, '"[^"]*"|\'[^\']*\'' ), FALSE)) {
    return(TRUE)
  }

  # If none of the above rules apply, default to numeric.
  # This implies that if it's a variable reference, it's numeric unless explicitly string.
  # Or if it's a simple arithmetic expression, it's numeric.
  return(FALSE)
}
```
!END_MODIFICATION sfun_is_stata_expression_string_typed sfun_is_stata_expression_string_typed.R


!MODIFICATION translate_stata_expression_to_r stata_expression_translator.R
scope = "function"
file = "R/stata_expression_translator.R"
function_name = "translate_stata_expression_to_r"
description = "Fix translation of ~= and standalone ~ logical NOT operator, and add it to NA comparison handling"
---
```r
translate_stata_expression_to_r = function(stata_expr, context = list(is_by_group = FALSE), r_value_mappings = NULL) {
  restore.point("translate_stata_expression_to_r")

  if (is.null(stata_expr) || length(stata_expr) == 0 || !is.character(stata_expr)) {
    stata_expr = NA_character_
  } else {
    stata_expr = as.character(stata_expr[1])
  }

  if (is.na(stata_expr) || stata_expr == "") {
    return("NA_real_")
  }

  r_expr = stata_expr

  # Handle Stata functions that are fragile with direct lookup inside mutate.
  # We translate these first into local wrappers that bind their arguments in
  # the current evaluation context before calling the runtime helper.
  r_expr = s2r_replace_special_function_calls(
    r_expr,
    "inlist",
    function(args_str) {
      s2r_translate_inlist_call(
        args_str,
        context = context,
        r_value_mappings = r_value_mappings
      )
    }
  )

  r_expr = s2r_replace_special_function_calls(
    r_expr,
    "inrange",
    function(args_str) {
      s2r_translate_inrange_call(
        args_str,
        context = context,
        r_value_mappings = r_value_mappings
      )
    }
  )

  # --- Handle string literals by replacing them with unique placeholders ---
  string_literal_map = list()
  placeholder_counter = 0
  literal_matches_list = stringi::stri_match_all_regex(r_expr, '"[^"]*"|\'[^\']*\'')
  if (length(literal_matches_list) > 0 && !is.null(literal_matches_list[[1]]) &&
      NROW(literal_matches_list[[1]]) > 0 && !is.na(literal_matches_list[[1]][1,1])) {
    unique_literals = unique(literal_matches_list[[1]][,1])
    for (literal_text in unique_literals) {
      placeholder_counter = placeholder_counter + 1
      placeholder = paste0("_", placeholder_counter, "STATA2R_SLIT_")
      r_expr = stringi::stri_replace_all_fixed(r_expr, literal_text, placeholder)
      string_literal_map[[placeholder]] = literal_text
    }
  }

  # --- Handle r() and e() values using placeholders first ---
  macro_placeholder_map = list()
  macro_placeholder_counter = 0
  if (!is.null(r_value_mappings) && length(r_value_mappings) > 0) {
    sorted_macro_names = names(r_value_mappings)[order(stringi::stri_length(names(r_value_mappings)), decreasing = TRUE)]
    for (stata_macro_name in sorted_macro_names) {
      macro_placeholder_counter = macro_placeholder_counter + 1
      macro_placeholder = paste0("_", macro_placeholder_counter, "STATA2R_MACRO_")
      r_expr = stringi::stri_replace_all_fixed(r_expr, stata_macro_name, macro_placeholder)
      macro_placeholder_map[[macro_placeholder]] = r_value_mappings[[stata_macro_name]]
    }
  }

  # Step 1: Handle Stata missing value literals '.', '.a', ..., '.z'
  r_expr = stringi::stri_replace_all_regex(r_expr, "(?<![0-9a-zA-Z_])\\.[a-zA-Z]?(?![0-9a-zA-Z_])", "NA_real_")

  # Step 2: Translate Stata logical operators and missing value comparisons.
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\b[a-zA-Z_][a-zA-Z0-9_.]*\\b)\\s*==\\s*NA_real_", "sfun_missing($1)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\b[a-zA-Z_][a-zA-Z0-9_.]*\\b)\\s*(?:!=|~=)\\s*NA_real_", "!sfun_missing($1)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "(?<![<>=!~])\\s*=\\s*(?![=])", " == ")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\s*~=\\s*", " != ")
  r_expr = stringi::stri_replace_all_regex(r_expr, "(?<![a-zA-Z0-9_\\.])~", "!")

  # Step 3: Translate Stata special variables and indexing (e.g., _n, _N, var[_n-1])
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\w+)\\[_n\\s*-\\s*(\\d+)\\]", "dplyr::lag(`$1`, n = $2)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\w+)\\[_n\\s*\\+\\s*(\\d+)\\]", "dplyr::lead(`$1`, n = $2)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\w+)\\[_n\\]", "`$1`")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\b_n\\b", "dplyr::row_number()")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\b_N\\b", "dplyr::n()")

  # Step 4: Iteratively translate Stata functions (excluding inlist/inrange, handled above)
  old_r_expr = ""
  while (fast_coalesce(r_expr != old_r_expr, FALSE)) {
    old_r_expr = r_expr
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bcond\\(([^,]+),([^,]+),([^)]+)\\)", "sfun_stata_cond($1, $2, $3)")
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
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\byear\\(([^)]+)\\)", "sfun_year($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bmonth\\(([^)]+)\\)", "sfun_month($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bday\\(([^)]+)\\)", "sfun_day($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bqofd\\(([^)]+)\\)", "sfun_qofd($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bdow\\(([^)]+)\\)", "sfun_dow($1)")
  }

  if (is.na(r_expr) || r_expr == "") {
    warning(paste0("R expression became NA or empty after function translation. Original Stata expression: '", stata_expr, "'"))
    return("NA_real_")
  }

  # Step 5: Quote bare variable names with backticks
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
    "sfun_month", "sfun_qofd", "sfun_dow", "sfun_inlist", "sfun_inrange", "sfun_normalize_string_nas", "sfun_strip_stata_attributes",
    "sfun_compress_col_type", "sfun_is_stata_expression_string_typed", "as.logical",
    "sfun_stata_cond", "sfun_year", "sfun_stata_date_single", "e", "sfun_flag", "sfun_fdiff",
    "NROW", "length", "unique", "sapply", "vapply", "c", "list", "intersect", "setdiff",
    "warning", "stop", "paste0", "grepl", "as.logical", "ifelse", "exists", "rm",
    "is.null", "lapply", "is.na", "is.character", "is.numeric", "is.logical", "is.factor",
    "attributes", "attr", "names", "order", "unname", "duplicated", "trimws",
    "suppressWarnings", "as.data.frame", "rownames", "colnames", "head", "tail",
    "matrix", "data.frame", "vector", "character", "numeric", "integer", "logical",
    "factor", "double", "`_n`", "`_N`",
    "cur_group_id", "cur_data_all", "replace", "TRUE", "FALSE",
    "local"
  )

  locations_list = stringi::stri_locate_all_regex(r_expr, "\\b([a-zA-Z_][a-zA-Z0-9_.]*)\\b")
  locations = locations_list[[1]]

  if (!is.null(locations) && NROW(locations) > 0 && !is.na(locations[1,1])) {
    locations = locations[order(locations[,2], decreasing = TRUE), , drop = FALSE]
    for (k in seq_len(NROW(locations))) {
      start_pos = locations[k,1]
      end_pos = locations[k,2]
      current_word = stringi::stri_sub(r_expr, start_pos, end_pos)

      if (current_word %in% names(string_literal_map)) {
        next
      }
      if (current_word %in% names(macro_placeholder_map)) {
        next
      }

      is_reserved = fast_coalesce(current_word %in% r_reserved_words, FALSE)
      is_numeric_literal = fast_coalesce(suppressWarnings(!is.na(as.numeric(current_word))), FALSE)

      is_already_backticked = FALSE
      if (fast_coalesce(start_pos > 1 && end_pos < stringi::stri_length(r_expr), FALSE)) {
        char_before = fast_coalesce(stringi::stri_sub(r_expr, start_pos - 1, start_pos - 1), "")
        char_after = fast_coalesce(stringi::stri_sub(r_expr, end_pos + 1, end_pos + 1), "")
        is_already_backticked = (char_before == "`" && char_after == "`")
      }

      if (isTRUE(!is_reserved) && isTRUE(!is_numeric_literal) && isTRUE(!is_already_backticked)) {
        r_expr = paste0(
          stringi::stri_sub(r_expr, 1, start_pos - 1),
          "`", current_word, "`",
          stringi::stri_sub(r_expr, end_pos + 1, stringi::stri_length(r_expr))
        )
      }
    }
  }

  # Step 6: Translate Stata '+' operator to sfun_stata_add for polymorphic behavior
  operand_pattern = "(?:\"[^\"]*\"|'[^']*'|\\d+(?:\\.\\d+)?(?:e[+-]?\\d+)?|\\b(?:NA_real_|NULL)\\b|\\b(?:TRUE|FALSE)\\b|`[^`]+`|\\b[a-zA-Z_][a-zA-Z0-9_.]*\\s*\\(.*?\\)\\s*|_[0-9]+STATA2R_SLIT_|_[0-9]+STATA2R_MACRO_)"
  old_r_expr_add = ""
  while (fast_coalesce(r_expr != old_r_expr_add, FALSE)) {
    old_r_expr_add = r_expr
    add_regex_middle_part = "\\s*(?<![<>=!~])\\+\\s*(?!\\s*\\+|\\s*=\\s*)"
    add_regex_full = paste0("(", operand_pattern, ")", add_regex_middle_part, "(", operand_pattern, ")")
    r_expr = stringi::stri_replace_all_regex(r_expr, add_regex_full, "sfun_stata_add($1, $2)")
  }

  # --- Restore mapped macro expressions from placeholders ---
  if (length(macro_placeholder_map) > 0) {
    sorted_macro_placeholders = names(macro_placeholder_map)[order(stringi::stri_length(names(macro_placeholder_map)), names(macro_placeholder_map), decreasing = TRUE)]
    for (macro_placeholder in sorted_macro_placeholders) {
      r_expr = stringi::stri_replace_all_fixed(r_expr, macro_placeholder, macro_placeholder_map[[macro_placeholder]])
    }
  }

  # --- Restore string literals from placeholders ---
  if (length(string_literal_map) > 0) {
    sorted_placeholders = names(string_literal_map)[order(stringi::stri_length(names(string_literal_map)), names(string_literal_map), decreasing = TRUE)]
    for (placeholder in sorted_placeholders) {
      r_expr = stringi::stri_replace_all_fixed(r_expr, placeholder, string_literal_map[[placeholder]])
    }
  }

  return(r_expr)
}
```
!END_MODIFICATION translate_stata_expression_to_r stata_expression_translator.R

!MODIFICATION scmd_generate t_generate.R
scope = "function"
file = "R/t_generate.R"
function_name = "scmd_generate"
description = "Apply abbreviation expansion to expressions before evaluation"
---
```r
scmd_generate = function(data, new_var, r_expr_str, r_if_cond = NA_character_, group_vars = character(0), is_string = FALSE, force_integer = FALSE) {
  restore.point("scmd_generate")

  r_expr_str = resolve_abbrevs_in_expr(r_expr_str, names(data))
  r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))

  expr_val = r_expr_str
  if (is_string) {
    if (expr_val == "NA_real_") expr_val = '""' else expr_val = paste0("as.character(", expr_val, ")")
  } else {
    if (force_integer) expr_val = paste0("as.integer(", expr_val, ")") else expr_val = paste0("as.numeric(", expr_val, ")")
  }

  na_val = if (is_string) '""' else "NA_real_"
  if (!is.na(r_if_cond) && r_if_cond != "") {
    expr_val = paste0("dplyr::if_else((fast_coalesce(as.numeric(", r_if_cond, "), 0) != 0), ", expr_val, ", ", na_val, ")")
  }

  pipe_el = c("data")
  group_vars = expand_varlist(paste(group_vars, collapse=" "), names(data))
  if (length(group_vars) > 0) pipe_el = c(pipe_el, paste0("dplyr::group_by(!!!dplyr::syms(c('", paste(group_vars, collapse="','"), "')))"))
  pipe_el = c(pipe_el, paste0("dplyr::mutate(`", new_var, "` = ", expr_val, ")"))
  if (length(group_vars) > 0) pipe_el = c(pipe_el, "dplyr::ungroup()")

  # Evaluate code inside parent.frame() to capture previous steps' variables
  eval(parse(text = paste(pipe_el, collapse = " %>% ")), envir = list(data = data), enclos = parent.frame())
}
```
!END_MODIFICATION scmd_generate t_generate.R

!MODIFICATION scmd_replace t_replace.R
scope = "function"
file = "R/t_replace.R"
function_name = "scmd_replace"
description = "Apply abbreviation expansion to expressions before evaluation"
---
```r
scmd_replace = function(data, var_to_replace, r_expr_str, r_if_cond = NA_character_, group_vars = character(0), is_string = FALSE, force_integer = FALSE) {
  restore.point("scmd_replace")

  var_actual = expand_varlist(var_to_replace, names(data))[1]

  r_expr_str = resolve_abbrevs_in_expr(r_expr_str, names(data))
  r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))

  expr_val = r_expr_str
  if (is_string) {
    if (expr_val == "NA_real_") expr_val = '""' else expr_val = paste0("as.character(", expr_val, ")")
  } else {
    if (force_integer) expr_val = paste0("as.integer(", expr_val, ")") else expr_val = paste0("as.numeric(", expr_val, ")")
  }

  if (!is.na(r_if_cond) && r_if_cond != "") {
    expr_val = paste0("dplyr::if_else((fast_coalesce(as.numeric(", r_if_cond, "), 0) != 0), ", expr_val, ", data$`", var_actual, "`)")
  }

  pipe_el = c("data")
  group_vars = expand_varlist(paste(group_vars, collapse=" "), names(data))
  if (length(group_vars) > 0) pipe_el = c(pipe_el, paste0("dplyr::group_by(!!!dplyr::syms(c('", paste(group_vars, collapse="','"), "')))"))
  pipe_el = c(pipe_el, paste0("dplyr::mutate(`", var_actual, "` = ", expr_val, ")"))
  if (length(group_vars) > 0) pipe_el = c(pipe_el, "dplyr::ungroup()")

  eval(parse(text = paste(pipe_el, collapse = " %>% ")), envir = list(data = data), enclos = parent.frame())
}
```
!END_MODIFICATION scmd_replace t_replace.R

!MODIFICATION scmd_egen t_egen.R
scope = "function"
file = "R/t_egen.R"
function_name = "scmd_egen"
description = "Apply abbreviation expansion to expressions before evaluation"
---
```r
scmd_egen = function(data, new_var, func_name, calc_expr, group_vars = character(0), args_str = NA_character_, needs_temp_sort = FALSE, is_row = FALSE) {
  restore.point("scmd_egen")

  group_vars_actual = expand_varlist(paste(group_vars, collapse=" "), names(data))
  if (func_name %in% c("group", "tag")) {
    arg_vars = expand_varlist(args_str, names(data))
    group_vars_actual = unique(c(group_vars_actual, arg_vars))
  }

  if (is_row) {
    row_vars = expand_varlist(args_str, names(data))
    if (func_name == "rowtotal") {
      calc_expr = paste0("base::rowSums(replace(dplyr::select(data, dplyr::all_of(c('", paste(row_vars, collapse="','"), "'))), is.na(dplyr::select(data, dplyr::all_of(c('", paste(row_vars, collapse="','"), "')))), 0), na.rm = FALSE)")
    } else if (func_name == "rowmean") {
      calc_expr = paste0("base::rowMeans(dplyr::select(data, dplyr::all_of(c('", paste(row_vars, collapse="','"), "'))), na.rm = TRUE)")
    } else if (func_name == "concat") {
      na_checks = paste0("is.na(data[['", row_vars, "']])", collapse=" & ")
      stri_args = paste0("dplyr::if_else(is.na(as.character(data[['", row_vars, "']])), \"\", as.character(data[['", row_vars, "']]))", collapse=", ")
      calc_expr = paste0("dplyr::if_else(", na_checks, ", NA_character_, stringi::stri_paste(", stri_args, ", sep = ''))")
    }
  } else {
    calc_expr = resolve_abbrevs_in_expr(calc_expr, names(data))
  }

  if (needs_temp_sort) {
    tmp = data
    sort_vars = group_vars_actual
    if (func_name == "rank") sort_vars = unique(c(sort_vars, expand_varlist(args_str, names(data))))
    if ("stata2r_original_order_idx" %in% names(tmp)) sort_vars = c(sort_vars, "stata2r_original_order_idx")

    if (length(sort_vars) > 0) {
      sort_cmd = paste0("dplyr::arrange(tmp, ", paste(paste0("`", sort_vars, "`"), collapse=", "), ")")
      tmp = eval(parse(text = sort_cmd), envir = list(tmp = tmp), enclos = parent.frame())
    }

    pipe_el = c("tmp")
    if (length(group_vars_actual) > 0) pipe_el = c(pipe_el, paste0("dplyr::group_by(!!!dplyr::syms(c('", paste(group_vars_actual, collapse="','"), "')))"))
    pipe_el = c(pipe_el, paste0("dplyr::mutate(`", new_var, "` = ", calc_expr, ")"))
    if (length(group_vars_actual) > 0) pipe_el = c(pipe_el, "dplyr::ungroup()")

    tmp = eval(parse(text = paste(pipe_el, collapse = " %>% ")), envir = list(tmp = tmp), enclos = parent.frame())

    if ("stata2r_original_order_idx" %in% names(data)) {
      data = dplyr::left_join(data, tmp[, c("stata2r_original_order_idx", new_var)], by = "stata2r_original_order_idx")
    } else {
      data[[new_var]] = tmp[[new_var]]
    }

  } else {
    pipe_el = c("data")
    if (length(group_vars_actual) > 0 && !is_row) pipe_el = c(pipe_el, paste0("dplyr::group_by(!!!dplyr::syms(c('", paste(group_vars_actual, collapse="','"), "')))"))
    pipe_el = c(pipe_el, paste0("dplyr::mutate(`", new_var, "` = ", calc_expr, ")"))
    if (length(group_vars_actual) > 0 && !is_row) pipe_el = c(pipe_el, "dplyr::ungroup()")
    data = eval(parse(text = paste(pipe_el, collapse = " %>% ")), envir = list(data = data), enclos = parent.frame())
  }

  return(data)
}
```
!END_MODIFICATION scmd_egen t_egen.R

!MODIFICATION scmd_recode t_recode.R
scope = "function"
file = "R/t_recode.R"
function_name = "scmd_recode"
description = "Apply abbreviation expansion to expressions before evaluation"
---
```r
scmd_recode = function(data, varlist_str, rules_templates, gen_vars_str = NA_character_, r_if_cond = NA_character_, is_string = FALSE, labels_map = NULL) {
  restore.point("scmd_recode")

  vars_actual = expand_varlist(varlist_str, names(data))

  new_vars = vars_actual
  if (!is.na(gen_vars_str)) {
    new_vars = stringi::stri_split_regex(gen_vars_str, "\\s+")[[1]]
    new_vars = new_vars[new_vars != ""]
    if (length(new_vars) != length(vars_actual)) stop("scmd_recode: gen() requires same number of vars.")
  }

  r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))

  for (i in seq_along(vars_actual)) {
    old_var = vars_actual[i]
    new_var = new_vars[i]

    old_attrs = attributes(data[[old_var]])

    r_rules = gsub(".VAR.", paste0("`", old_var, "`"), rules_templates, fixed = TRUE)
    # Also resolve abbreviations in rules
    r_rules = vapply(r_rules, function(r) resolve_abbrevs_in_expr(r, names(data)), character(1))

    # STATA FALLBACK: If values do not match any conditions, they are left unchanged.
    fallback = if (is_string) paste0("as.character(`", old_var, "`)") else paste0("as.numeric(`", old_var, "`)")
    r_rules = c(r_rules, paste0("TRUE ~ ", fallback))

    case_when_expr = paste0("dplyr::case_when(\n    ", paste(r_rules, collapse = ",\n    "), "\n  )")

    if (!is.na(r_if_cond) && r_if_cond != "") {
      final_val_expr = paste0("dplyr::if_else((fast_coalesce(as.numeric(", r_if_cond, "), 0) != 0), ", case_when_expr, ", `", old_var, "`)")
    } else {
      final_val_expr = case_when_expr
    }

    if (is_string) {
      final_val_expr = paste0("as.character(", final_val_expr, ")")
    } else {
      final_val_expr = paste0("as.numeric(", final_val_expr, ")")
    }

    data = eval(parse(text = paste0("dplyr::mutate(data, `", new_var, "` = ", final_val_expr, ")")), envir = list(data = data), enclos = parent.frame())

    # Restore or Assign Labels
    if (!is.null(labels_map)) {
      data[[new_var]] = haven::labelled(data[[new_var]], labels = labels_map)
    } else if (old_var == new_var && !is_string) {
      # If replacing and no new labels are provided, keep original labels matching Stata
      if (!is.null(old_attrs$labels)) attr(data[[new_var]], "labels") = old_attrs$labels
      if (!is.null(old_attrs$label)) attr(data[[new_var]], "label") = old_attrs$label
      if (!is.null(old_attrs$class) && "haven_labelled" %in% old_attrs$class) {
        class(data[[new_var]]) = old_attrs$class
      }
    }
  }

  return(data)
}
```
!END_MODIFICATION scmd_recode t_recode.R

!MODIFICATION scmd_collapse t_collapse.R
scope = "function"
file = "R/t_collapse.R"
function_name = "scmd_collapse"
description = "Apply abbreviation expansion to expressions before evaluation"
---
```r
scmd_collapse = function(data, agg_exprs_list, group_vars = character(0), r_if_cond = NA_character_) {
  restore.point("scmd_collapse")

  r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))

  if (!is.na(r_if_cond) && r_if_cond != "") {
    data = data[s2r_eval_cond(data, r_if_cond, envir = parent.frame()), , drop = FALSE]
  }

  pipe_el = c("data")
  group_vars_actual = expand_varlist(paste(group_vars, collapse = " "), names(data))
  if (length(group_vars_actual) > 0) {
    pipe_el = c(
      pipe_el,
      paste0("collapse::fgroup_by(", paste(group_vars_actual, collapse = ", "), ")")
    )
  }

  agg_exprs_list = lapply(agg_exprs_list, function(expr) resolve_abbrevs_in_expr(expr, names(data)))

  agg_str = paste(
    sprintf("`%s` = %s", names(agg_exprs_list), unlist(agg_exprs_list)),
    collapse = ", "
  )
  pipe_el = c(pipe_el, paste0("collapse::fsummarise(", agg_str, ")"))
  if (length(group_vars_actual) > 0) {
    pipe_el = c(pipe_el, "collapse::fungroup()")
  }

  data = eval(
    parse(text = paste(pipe_el, collapse = " %>% ")),
    envir = list(data = data),
    enclos = parent.frame()
  )
  data$stata2r_original_order_idx = seq_len(nrow(data))
  return(data)
}
```
!END_MODIFICATION scmd_collapse t_collapse.R
