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

  # Step 1: Translate Stata logical operators and missing value comparisons
  # These must happen before handling `.` to `NA_real_` because `X == .` is special.
  # Stata `X == .` -> R `is.na(X)`
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\b[a-zA-Z_][a-zA-Z0-9_.]*\\b)\\s*==\\s*\\.", "is.na($1)")
  # Stata `X != .` -> R `!is.na($1)`
  r_expr = stringi::stri_all_regex_replace(r_expr, "(\\b[a-zA-Z_][a-zA-Z0-9_.]*\\b)\\s*!=\\s*\\.", "!is.na($1)") # Use stri_all_regex_replace for safety

  r_expr = stringi::stri_replace_all_regex(r_expr, "(?<![<>!=~])\\s*=\\s*(?![=])", " == ") # Replace single = with == if not part of other ops
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\s+~=\\s+", " != ") # Stata `~=` to R `!=`


  # Step 2: Translate Stata special variables and indexing (e.g., _n, _N, var[_n-1])
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

  # Handle Stata missing value literal '.' (now that X == . and X != . are handled)
  # Refined regex: match a dot not preceded or followed by a digit.
  r_expr = stringi::stri_replace_all_regex(r_expr, "(?<!\\d)\\.(?!\\d)", "NA_real_")

  # Step 3: Iteratively translate Stata functions (e.g., cond(), round(), log(), etc.)
  # This loop handles nested function calls by repeatedly applying transformations.
  old_r_expr = ""
  while (r_expr != old_r_expr) {
    old_r_expr = r_expr

    # Apply more specific regexes first if there are overlaps (e.g., round(x,y) before round(x))
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bcond\\(([^,]+),([^,]+),([^)]+)\\)", "dplyr::if_else($1, $2, $3)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bround\\(([^,]+),([^)]+)\\)", "sfun_stata_round($1, $2)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bround\\(([^)]+)\\)", "sfun_stata_round($1, 1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bmod\\(([^,]+),([^)]+)\\)", "($1 %% $2)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bmissing\\(([^)]+)\\)", "is.na($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\blog\\(([^)]+)\\)", "log($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bsqrt\\(([^)]+)\\)", "sqrt($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bint\\(([^)]+)\\)", "trunc($1)") # Stata int() truncates

    # String functions (using stringi or custom helpers)
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstrtrim\\(([^)]+)\\)", "stringi::stri_trim_both($1)")
    # Corrected stritrim translation: now calls sfun_stritrim
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstritrim\\(([^)]+)\\)", "sfun_stritrim($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\blower\\(([^)]+)\\)", "stringi::stri_trans_tolower($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bupper\\(([^)]+)\\)", "stringi::stri_trans_toupper($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bsubstr\\(([^,]+),([^,]+),([^)]+)\\)", "stringi::stri_sub($1, from = $2, length = $3)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bsubinstr\\(([^,]+),([^,]+),([^,]+),([^)]+)\\)", "sfun_subinstr($1, $2, $3, $4)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstrpos\\(([^,]+),([^)]+)\\)", "sfun_strpos($1, $2)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\blength\\(([^)]+)\\)", "stringi::stri_length($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstrlen\\(([^)]+)\\)", "stringi::stri_length($1)")
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstring\\(([^)]+)\\)", "as.character($1)")
    # Random number generator functions
    r_expr = stringi::stri_replace_all_regex(r_expr, "\\bruniform\\(\\)", "stats::runif(as.numeric(dplyr::n()))") # Stata runiform()
    # Date functions (placeholder, as actual implementation is complex)
  }

  # Step 4: Handle r() values using the mapping
  if (!is.null(r_value_mappings) && length(r_value_mappings) > 0) {
    for (stata_r_name in names(r_value_mappings)) {
      # Remove word boundaries (\\b) for r() values.
      # r() is not a word boundary, so \\b prevents proper matching.
      # stata_r_regex already escapes parentheses.
      stata_r_regex = gsub("(", "\\(", gsub(")", "\\)", stata_r_name, fixed=TRUE), fixed=TRUE)
      r_expr = stringi::stri_replace_all_regex(r_expr, stata_r_regex, r_value_mappings[[stata_r_name]])
    }
  }

  # Step 5: Translate Stata '+' operator to sfun_stata_add for polymorphic behavior
  # This needs to be applied iteratively until no more '+' signs (that are not part of comparison operators) exist.
  # The regex for operands must be robust to capture complete R expressions, including nested function calls.

  # Define a robust pattern for an R expression unit that can be an operand.
  # This pattern matches:
  # 1. A variable name (starts with letter/underscore, then alphanumeric/underscore/dot)
  # 2. A numeric literal
  # 3. A string literal (single or double quoted)
  # 4. A function call (e.g., `log(x)`, `sfun_stata_add(a, b)`), allowing for one level of nested simple function calls
  #    within its arguments, for robustness. This is a pragmatic balance for regex complexity.
  #    The regex `\\b\\w+\\((?:[^()]|\\b\\w+\\([^()]*\\))*\\)` is designed to handle `f(arg)` and `f(g(arg))`
  #    but not deeper nesting like `f(g(h(arg)))`. Given Stata's expression complexity, this should be sufficient for most cases.
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

# Helper to find the R variable name for a Stata r() value like "r(mean)"
# Scans cmd_df backwards from current_line_index - 1
# Looks for commands that set r() values (e.g., summarize)
# Constructs the R variable name based on the line number of that command
# Example: r(mean) set by summarize on line 5 becomes "stata_r_val_L5_mean"
get_r_value_mappings = function(stata_r_value_str, current_line_index, cmd_df) {
  restore.point("get_r_value_mappings")
  # stata_r_value_str is like "r(mean)", "r(N)", "r(sum)"
  # Extract the stat name, e.g. "mean" from "r(mean)"
  stat_name_match = stringi::stri_match_first_regex(stata_r_value_str, "r\\(([^)]+)\\)")
  if (is.na(stat_name_match[1,1])) return(NULL) # Not a valid r() syntax

  stat_name = stat_name_match[1,2]

  # Relevant commands that set r() values (this list can be expanded)
  r_setting_cmds = c("summarize", "su", "tabulate", "correlate", "count") # etc.

  # Scan backwards
  for (i in (current_line_index - 1):1) {
    # Check if the command was a summarize/su and if it set r() values.
    # We assume t_summarize sets r() values with the convention `stata_r_val_L<line>_<stat_name>`.
    # The actual variable names created depend on the `t_summarize` implementation.
    if (cmd_df$stata_cmd[i] %in% r_setting_cmds) {
      # Found a relevant command.
      # The R variable name is constructed based on this line index and stat_name.
      # This is a convention that t_summarize (and others) must follow.
      r_var_name = paste0("stata_r_val_L", cmd_df$line[i], "_", stat_name)

      # Return a list that translate_stata_expression_to_r can use
      # Mapping: "r(stat)" -> "generated_r_variable_name"
      mapping = list()
      mapping[[stata_r_value_str]] = r_var_name
      return(mapping)
    }
  }
  return(NULL) # No preceding r-setting command found for this stat
}

# Helper to extract all `r(...)` tokens from an expression
extract_r_values_from_expr = function(stata_expr) {
  restore.point("extract_r_values_from_expr")
  if (is.na(stata_expr)) return(character(0))
  unique(stringi::stri_match_all_regex(stata_expr, "\\br\\([^)]+\\)")[[1]][,1])
}

# Main function to translate an expression potentially containing r() values
translate_stata_expression_with_r_values = function(stata_expr, current_line_index, cmd_df, context = list(is_by_group = FALSE)) {
  restore.point("translate_stata_expression_with_r_values")
  if (is.na(stata_expr)) return(NA_character_)

  all_r_tokens = extract_r_values_from_expr(stata_expr)
  final_r_value_mappings = list()

  if (length(all_r_tokens) > 0) {
    for (r_token in all_r_tokens) {
      if (!is.na(r_token)) {
         current_mapping = get_r_value_mappings(r_token, current_line_index, cmd_df)
         if (!is.null(current_mapping)) {
           final_r_value_mappings = c(final_r_value_mappings, current_mapping)
         } else {
           # No mapping found, could be an error or r_token not from summarize etc.
           # For now, leave it as is or raise warning
           warning(paste("Could not find source for r-value:", r_token, "at line", cmd_df$line[current_line_index])) # Use cmd_df$line for actual line
         }
      }
    }
  }

  translate_stata_expression_to_r(stata_expr, context, final_r_value_mappings)
}

