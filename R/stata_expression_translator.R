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
  # In R, comparisons with NA are NA. Stata: `if var == .` is true if var is `.`.
  # `is.na(var)` is the R equivalent of `missing(var)`.
  # `var == .` -> `is.na(var)`
  # `var != .` -> `!is.na(var)`
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\b_n\\b", if(context$is_by_group) "dplyr::row_number()" else "dplyr::row_number(dplyr::across(dplyr::everything()))") # Simplification, needs actual row index
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\b_N\\b", if(context$is_by_group) "dplyr::n()" else "NROW(data)")


  # Stata functions to R functions
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bcond\\(([^,]+),([^,]+),([^)]+)\\)", "dplyr::if_else($1, $2, $3)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bmissing\\(([^)]+)\\)", "is.na($1)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\blog\\(([^)]+)\\)", "log($1)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bsqrt\\(([^)]+)\\)", "sqrt($1)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bint\\(([^)]+)\\)", "trunc($1)") # Stata int() truncates
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bround\\(([^,]+),([^)]+)\\)", "(round($1 / $2) * $2)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bround\\(([^)]+)\\)", "round($1)") # round(x) is round(x,1) in Stata
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bmod\\(([^,]+),([^)]+)\\)", "($1 %% $2)")


  # String functions (using stringi)
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstrtrim\\(([^)]+)\\)", "stringi::stri_trim_both($1)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstritrim\\(([^)]+)\\)", "stringi::stri_squish($1)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\blower\\(([^)]+)\\)", "stringi::stri_trans_tolower($1)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bupper\\(([^)]+)\\)", "stringi::stri_trans_toupper($1)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bsubstr\\(([^,]+),([^,]+),([^)]+)\\)", "stringi::stri_sub($1, from = $2, length = $3)")
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bsubinstr\\(([^,]+),([^,]+),([^,]+),([^)]+)\\)", "stringi::stri_replace_first_fixed($1, $2, $3)") # Simplified: assumes count is 1
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\bstrpos\\(([^,]+),([^)]+)\\)", "stringi::stri_locate_first_fixed($1, $2)[,1]") # strpos returns 0 if not found
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
  # `dow(d)` -> `as.integer(format(d, "%w"))`

  # Logical operators
  # Stata: & | ~ (or !) == ~= >= <= > <
  # R:     & | !        == != >= <= > <
  # `=` for equality in Stata expressions is `==` in R
  # Need to be careful not to replace inside strings or variable names
  # This requires more robust parsing than simple regex. For now, assume users write `==`.
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\s=\\\s", " == ") # Stata `=` in conditions
  r_expr = stringi::stri_replace_all_regex(r_expr, "\\s~=\\s", " != ") # Stata `~=`

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
  # `if strvar == ""` should be `strvar == ""` or `is.na(strvar) | strvar == ""` if Stata treats NA strings and "" similarly
  # This part is complex. For now, assume direct translation of `.` in specific contexts.
  # `var == .` becomes `is.na(var)`.
  # `var != .` becomes `!is.na(var)`.
  # Careful: `round(var, .1)` is valid Stata.
  # Regex for `== .` and `!= .` should be space-sensitive: `\s*==\s*\.\b` and `\s*!=\s*\.\b`
  # Correctly: `(\w+)\s*==\s*\.\b` -> `is.na($1)`
  # `(\w+)\s*!=\s*\.\b` -> `!is.na($1)`
  # `(\w+)\s*~=\s*\.\b` -> `!is.na($1)` (Stata)
  # But this is better handled by `missing()` translation.
  # A direct `.` might appear in `recode age (<y_bin_473>18=1) (.=99)`.
  # For expressions, `missing(var)` is the primary way.

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
  r_setting_cmds = c("summarize", "tabulate", "correlate", "count") # etc.

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
           warning(paste("Could not find source for r-value:", r_token, "at line", current_line_index))
         }
      }
    }
  }

  translate_stata_expression_to_r(stata_expr, context, final_r_value_mappings)
}


