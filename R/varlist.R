# Runtime helper functions for Stata variable list expansion

#' Expand Stata patterns (*, -, abbreviations) into actual column names at runtime
#' @param varlist_str Character string of the Stata varlist
#' @param data_cols Character vector of available columns in the dataset
expand_varlist = function(varlist_str, data_cols) {
  if (is.na(varlist_str) || trimws(varlist_str) == "") return(character(0))

  # Tokenize by whitespace
  tokens = stringi::stri_split_regex(trimws(varlist_str), "\\s+")[[1]]
  tokens = tokens[tokens != ""]

  expanded_vars = character(0)

  for (tok in tokens) {
    if (grepl("-", tok, fixed = TRUE)) {
      # Range: var1-var5
      parts = strsplit(tok, "-", fixed = TRUE)[[1]]
      if (length(parts) == 2 && all(parts %in% data_cols)) {
        idx1 = match(parts[1], data_cols)
        idx2 = match(parts[2], data_cols)
        # Ensure order is correct regardless of dataset column order
        range_idx = if (idx1 <= idx2) idx1:idx2 else idx1:idx2
        expanded_vars = c(expanded_vars, data_cols[range_idx])
      } else {
        expanded_vars = c(expanded_vars, tok) # fallback, might error later
      }
    } else if (grepl("\\*|\\?", tok)) {
      # Wildcards: var* or v?r
      rx = glob2rx(tok)
      matches = data_cols[grepl(rx, data_cols)]
      expanded_vars = c(expanded_vars, matches)
    } else {
      # Exact match or abbreviation
      if (tok %in% data_cols) {
        expanded_vars = c(expanded_vars, tok)
      } else {
        # Try abbreviation
        mcols = data_cols[startsWith(data_cols, tok)]
        if (length(mcols) == 1) {
          expanded_vars = c(expanded_vars, mcols[1])
        } else if (length(mcols) > 1) {
          # Matches multiple (ambiguous in strict Stata, but we take first or all depending on rules)
          # Stata errors on ambiguous abbreviation. We take the first for safety or error.
          expanded_vars = c(expanded_vars, mcols[1])
        } else {
          expanded_vars = c(expanded_vars, tok) # leave as is
        }
      }
    }
  }

  return(unique(expanded_vars))
}
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
