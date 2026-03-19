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
