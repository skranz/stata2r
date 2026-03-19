# FILE: R/stata_na.R

#' Helper to transform NA to Stata's encoding for conditions
#' In Stata, numeric missing (.) is treated as positive infinity.
to_stata_na = function(x) {
  if (is.numeric(x) || inherits(x, "Date")) {
    # Coerce to plain double to avoid strict integer/labelled casting issues with Inf
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
  # 1. Apply to_stata_na to all columns
  eval_list = lapply(data, to_stata_na)

  # 2. Inject NA values as Inf to correctly evaluate comparisons (like id >= .)
  eval_list[["NA_real_"]] = Inf
  eval_list[["NA_integer_"]] = Inf
  eval_list[["NA_character_"]] = ""
  eval_list[["NA"]] = Inf

  # 3. Override sfun_missing to handle Inf as missing for evaluations
  eval_list[["sfun_missing"]] = function(x) {
    if (is.numeric(x)) {
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
  c("NA_real_", "NA_integer_", "NA_character_", "NA", "sfun_missing")
}
