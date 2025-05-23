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
  if (is.na(stata_expr) || stata_expr == "") return(NA_character_)

  r_expr = stata_expr

  # Handle Stata missing values: . == NA, .a, .b etc also NA
  # `is.na(var)` is the R equivalent of `missing(var)`.
  # `var == .` -> `is.na(var)`
  # `var != .` -> `!is.na(var)`

  # Stata _n (current obs number) and _N (total obs in group/dataset)
  # For expressions involving _n and _N within dplyr pipes, dplyr::row_number() and dplyr::n() are appropriate.
  # For collapse, within fmutate/fsummarise with fgroup_by, .i is row number in group, .N is group size.
  # We need to be careful if context$is_by_group from cmd_obj (parsed by prefix) is the sole determinant.
  # If using collapse, and context$is_by_group is TRUE, then .i and .N could be used.
  # Let's stick to dplyr::row_number() and dplyr::n() for wider applicability in expressions for now,
  # as translation functions (t_generate etc.) will construct the dplyr/collapse pipe.
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\b_n\\b", "dplyr::row_number()")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\b_N\\b", if(context$is_by_group) "dplyr::n()" else "NROW(data)") # NROW(data) for overall _N

  # Handle varname[_n-k] for lag and varname[_n+k] for lead
  # Example: x[_n-1] -> dplyr::lag(x, 1)
  # Example: x[_n+1] -> dplyr::lead(x, 1)
  # Regex: (\w+)\[_n\s*-\s*(\d+)\]  -> dplyr::lag($1, $2)
  # Regex: (\w+)\[_n\s*\+\s*(\d+)\] -> dplyr::lead($1, $2)
  # Regex: (\w+)\[_n\]             -> $1 (or $1[dplyr::row_number()] if explicit indexing needed, but usually direct var access means current row)
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\w+)\\[_n\\s*-\\s*(\\d+)\\]", "dplyr::lag($1, $2)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\w+)\\[_n\\s*\\+\s*(\\d+)\\]", "dplyr::lead($1, $2)")
  # Direct _n indexing like x[_n] is just x in R vector context.
  r_expr = stringi::stri_replace_all_regex(r_expr, "(\\w+)\\[_n\\]", "$1")


  # Stata functions to R functions
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bcond\\(([^,]+),([^,]+),([^)]+)\\)", "dplyr::if_else($1, $2, $3)") # Assumes types are compatible
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bmissing\\(([^)]+)\\)", "is.na($1)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\blog\\(([^)]+)\\)", "log($1)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bsqrt\\(([^)]+)\\)", "sqrt($1)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bint\\(([^)]+)\\)", "trunc($1)") # Stata int() truncates

  # Stata round(x) is round(x,1) and rounds .5 away from zero. round(x,y) rounds to nearest multiple of y.
  # Using sfun_stata_round (requires sfun_stata_round.R to be sourced/available)
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bround\\(([^,]+),([^)]+)\\)", "sfun_stata_round($1, $2)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bround\\(([^)]+)\\)", "sfun_stata_round($1, 1)")

  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bmod\\(([^,]+),([^)]+)\\)", "($1 %% $2)")


  # String functions (using stringi or custom helpers)
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstrtrim\\(([^)]+)\\)", "stringi::stri_trim_both($1)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstritrim\\(([^)]+)\\)", "stringi::stri_squish($1)") # Stata stritrim squishes and trims
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\blower\\(([^)]+)\\)", "stringi::stri_trans_tolower($1)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bupper\\(([^)]+)\\)", "stringi::stri_trans_toupper($1)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bsubstr\\(([^,]+),([^,]+),([^)]+)\\)", "stringi::stri_sub($1, from = $2, length = $3)")

  # subinstr(s1,s2,s3,n): Stata's n=. means all occurrences. n=k means k-th.
  # For simplicity, current version translates to replace first fixed for specific n (assumed 1).
  # stringi::stri_replace_first_fixed for n=1. stringi::stri_replace_all_fixed for n="." (all).
  # This needs more robust parsing of the 4th argument of subinstr.
  # Simplified: Assume if 4th arg is present and numeric 1, use first_fixed. If '.', use all_fixed.
  # Current regex only matches 3 args for subinstr, effectively making it replace first.
  # This pattern implies subinstr(s1, s2, s3) which means replace all occurrences of s2 with s3 in s1, return s1 if s2 is empty
  # Stata: subinstr(s1, s2, s3, n) - n is number of substitutions. n=. is all.
  # A common case subinstr(s, "old", "new", 1) -> stringi::stri_replace_first_fixed(s, "old", "new")
  # A common case subinstr(s, "old", "new", .) -> stringi::stri_replace_all_fixed(s, "old", "new")
  # The regex `\\bsubinstr\\(([^,]+),([^,]+),([^,]+),([^)]+)\\)` extracts 4 args. $4 is n.
  # For now, keep it simple, only first occurrence:
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bsubinstr\\(([^,]+),([^,]+),([^,]+),([^)]+)\\)", "stringi::stri_replace_first_fixed($1, $2, $3)") # Simplified: assumes count is 1, ignores $4

  # strpos(s1,s2) returns 0 if not found. Using sfun_strpos.
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstrpos\\(([^,]+),([^)]+)\\)", "sfun_strpos($1, $2)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\blength\\(([^)]+)\\)", "stringi::stri_length($1)") # Stata length() is alias for strlen()
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstrlen\\(([^)]+)\\)", "stringi::stri_length($1)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstring\\(([^)]+)\\)", "as.character($1)")


  # Date functions (basic, assumes date variables are already R Date objects or can be coerced)
  # This is a simplification. Stata date functions are complex.
  # `date(s, mask)` -> `as.Date(s, format=lookup_mask(mask))`
  # `year(d)` -> `as.integer(format(d, "%Y"))`
  # `month(d)` -> `as.integer(format(d, "%m"))`
  # `day(d)` -> `as.integer(format(d, "%d"))`
  # `qofd(d)` -> `lubridate::quarter(d)` (needs lubridate or custom)
  # `mdy(M,D,Y)` -> `as.Date(paste(Y,M,D,sep="-"))`
  # `dow(d)` -> `as.integer(format(d, "%w"))` # Stata 0=Sun, R %w 0=Sun

  # Logical operators
  # Stata: & | ~ (or !) == ~= >= <= > <
  # R:     & | !        == != >= <= > <
  # `=` for equality in Stata expressions is `==` in R.
  # `~=` for inequality is `!=`.
  # The regex needs to target single `=` used for comparison, not part of other operators.
  # And not assignment `=` which is handled by t_generate/t_replace parsing.
  r_expr = stringi::stri_replace_all_regex(r_expr, "(?<![<>!=~])\\s*=\\s*(?![=])", " == ") # Replace single = with == if not part of other ops
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\s+~=\\s+", " != ") # Stata `~=` to R `!=`

  # Handle r() values using the mapping
  if (!is.null(r_value_mappings) && length(r_value_mappings) > 0) {
    for (stata_r_name in names(r_value_mappings)) {
      # stata_r_name will be like "r(mean)", "r(sd)"
      # Need to escape parentheses for regex
      stata_r_regex = gsub("(", "\\(", gsub(")", "\\)", stata_r_name, fixed=TRUE), fixed=TRUE)
      r_expr = stringi::stri_replace_all_regex(r_expr, paste0("\\b", stata_r_regex, "\\b"), r_value_mappings[[stata_r_name]])
    }
  }

  # Stata `.` for missing numeric, `""` for missing string.
  # `if var == .` should be `is.na(var)`
  # `if strvar == ""` should be `strvar == ""` or `is.na(strvar) | strvar == ""`
  # `missing(var)` already translates to `is.na(var)`.
  # Direct comparison `var == .` (if not caught by above equality rule for general vars) should also be `is.na(var)`.
  # This needs to be careful not to misinterpret `.` in `round(var, .1)`.
  # The `missing()` translation is preferred. The `=` to `==` should handle `var = .` to `var == .`
  # then further refine `var == .` to `is.na(var)`.
  # This can be `stringi::stri_replace_all_regex(r_expr, "([a-zA-Z0-9_]+)\\s*==\\s*\\.", "is.na($1)")`
  # and `([a-zA-Z0-9_]+)\\s*!=\\s*\\.", "!is.na($1)")`
  # This might be too aggressive. `missing()` is safer.

  return(r_expr)
}

# Helper to find the R variable name for a Stata r() value like "r(mean)"
# Scans cmd_df backwards from current_line_index - 1
# Looks for commands that set r() values (e.g., summarize)
# Constructs the R variable name based on the line number of that command
# Example: r(mean) set by summarize on line 5 becomes "stata_r_val_L5_mean"
get_r_value_mappings = function(stata_r_value_str, current_line_index, cmd_df) {
  # stata_r_value_str is like "r(mean)", "r(N)", "r(sum)"
  # Extract the stat name, e.g. "mean" from "r(mean)"
  stat_name_match = stringi::stri_match_first_regex(stata_r_value_str, "r\\(([^)]+)\\)")
  if (is.na(stat_name_match[1,1])) return(NULL) # Not a valid r() syntax

  stat_name = stat_name_match[1,2]

  # Relevant commands that set r() values (this list can be expanded)
  r_setting_cmds = c("summarize", "su", "tabulate", "correlate", "count") # etc.

  # Scan backwards
  for (i in (current_line_index - 1):1) {
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
  if (is.na(stata_expr)) return(character(0))
  unique(stringi::stri_match_all_regex(stata_expr, "\\br\\([^)]+\\)")[[1]][,1])
}

# Main function to translate an expression potentially containing r() values
translate_stata_expression_with_r_values = function(stata_expr, current_line_index, cmd_df, context = list(is_by_group = FALSE)) {
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




