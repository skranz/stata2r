# FILE: R/stata_na.R

#' Convert R Date values to Stata daily-date numeric values
#'
#' Stata daily dates are stored as days since 1960-01-01.
#' R Date objects are internally days since 1970-01-01, so plain
#' as.numeric(Date) cannot be compared to values created by Stata-style
#' functions such as mdy() or date().
s2r_date_to_stata_numeric = function(x) {
  as.numeric(as.Date(x) - as.Date("1960-01-01"))
}

#' Helper to transform NA to Stata's encoding for conditions
#'
#' In Stata, numeric missing (.) is treated as positive infinity in
#' comparisons. String missing is the empty string.
to_stata_na = function(x) {
  if (inherits(x, "Date")) {
    x = s2r_date_to_stata_numeric(x)
    x[is.na(x)] = Inf
  } else if (is.numeric(x)) {
    x = as.numeric(x)
    x[is.na(x)] = Inf
  } else if (is.character(x)) {
    x[is.na(x)] = ""
  }
  x
}

#' Initialize an evaluation list with Stata NA rules
#' @param data The dataframe to base the evaluation list on
#' @return A list ready for `eval()` containing data and Stata NA mappings
s2r_setup_eval_list = function(data) {
  eval_list = lapply(data, to_stata_na)

  eval_list[["NA_real_"]] = Inf
  eval_list[["NA_integer_"]] = Inf
  eval_list[["NA_character_"]] = ""
  eval_list[["NA"]] = Inf

  # Allow expressions rewritten to .data[[...]] to work both in plain eval()
  # and in dplyr data-masked contexts.
  eval_list[[".data"]] = data

  eval_list[["sfun_missing"]] = function(x) {
    if (inherits(x, "Date")) {
      return(is.na(x))
    } else if (is.numeric(x)) {
      return(is.na(x) | is.infinite(x))
    } else if (is.character(x)) {
      return(is.na(x) | stringi::stri_trim_both(x) == "")
    }
    return(is.na(x))
  }

  return(eval_list)
}

#' List of reserved words injected by s2r_setup_eval_list
s2r_eval_reserved_words = function() {
  c("NA_real_", "NA_integer_", "NA_character_", "NA", ".data", "sfun_missing")
}

#' Convert R logical results to Stata-style 0/1 indicators
#'
#' Stata logical and comparison expressions used in generate/replace create
#' numeric 0/1 values. In particular, expressions like
#'   missing_numeric == 0
#' evaluate to 0 in Stata, while R returns NA.
#'
#' This helper is intentionally narrow: it should be applied to translated
#' logical/comparison expressions, not to arbitrary numeric expressions where
#' missing values should remain missing.
s2r_stata_logical = function(x) {
  if (is.logical(x)) {
    return(fast_coalesce(x, FALSE))
  }

  if (is.numeric(x)) {
    return(fast_coalesce(x != 0, FALSE))
  }

  fast_coalesce(as.logical(x), FALSE)
}
