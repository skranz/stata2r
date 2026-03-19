# FILE: R/if_in.R

#' Parse standard Stata if/in components from the end of a string
#' @param str The raw Stata command string (after options have been removed)
#' @return A list with `base_str`, `if_str`, and `in_str`
s2r_parse_if_in = function(str) {
  restore.point("s2r_parse_if_in")
  res = list(base_str = str, if_str = NA_character_, in_str = NA_character_)
  if (is.na(str) || trimws(str) == "") return(res)

  # Extract `in` condition
  in_match = stringi::stri_match_last_regex(res$base_str, "\\b(?:in)\\s+(.*)$")
  if (!is.na(in_match[1,1])) {
    res$in_str = stringi::stri_trim_both(in_match[1,2])
    res$base_str = stringi::stri_trim_both(stringi::stri_replace_last_fixed(res$base_str, in_match[1,1], ""))
  }

  # Extract `if` condition
  if_match = stringi::stri_match_last_regex(res$base_str, "\\b(?:if)\\s+(.*)$")
  if (!is.na(if_match[1,1])) {
    res$if_str = stringi::stri_trim_both(if_match[1,2])
    res$base_str = stringi::stri_trim_both(stringi::stri_replace_last_fixed(res$base_str, if_match[1,1], ""))
  }

  return(res)
}

#' Evaluate a Stata condition string at runtime, returning a boolean vector
s2r_eval_cond = function(data, r_cond) {
  restore.point("s2r_eval_cond")
  if (is.na(r_cond) || r_cond == "") return(rep(TRUE, NROW(data)))

  expr = base::parse(text = r_cond)
  used_vars = all.vars(expr)
  data_cols = names(data)

  eval_list = s2r_setup_eval_list(data)
  reserved = s2r_eval_reserved_words()

  for (v in used_vars) {
    if (!(v %in% c(data_cols, reserved))) {
      mcols = data_cols[startsWith(data_cols, v)]
      if (length(mcols) == 1) {
        eval_list[[v]] = to_stata_na(data[[mcols[1]]])
      } else if (length(mcols) > 1) {
        stop(paste("Ambiguous abbreviation in condition:", v))
      }
    }
  }

  cond_val = base::eval(expr, envir = eval_list, enclos = parent.frame())
  return(dplyr::coalesce(as.logical(cond_val), FALSE))
}

#' Evaluate a Stata 'in' range string at runtime, returning matched indices
s2r_eval_range = function(data, r_range) {
  restore.point("s2r_eval_range")
  if (is.na(r_range) || r_range == "") return(integer(0))

  idx = base::eval(base::parse(text = r_range))
  idx = idx[idx >= 1 & idx <= NROW(data)]
  return(idx)
}

#' Safely evaluate if/in conditions at runtime for row keeping
#' @param data The dataframe
#' @param r_if_cond The translated R condition string
#' @param r_in_range The translated R range string
#' @return The subsetted dataframe
s2r_eval_if_in = function(data, r_if_cond = NA_character_, r_in_range = NA_character_) {
  restore.point("seval_if_in")

  if (!is.na(r_if_cond) && r_if_cond != "") {
    cond_val = s2r_eval_cond(data, r_if_cond)
    data = data[cond_val, , drop = FALSE]
  }

  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    data = data[idx, , drop = FALSE]
  }

  return(data)
}

s2r_in_str_to_r_range_str = function(in_str) {
  if (is.null(in_str) | is.na(in_str)) return(NA_character_)

  range_match = stringi::stri_match_first_regex(in_str, "^(\\d+)(?:/(\\d+))?$")
  if (!is.na(range_match[1,1])) {
    start_row = range_match[1,2]
    end_row = range_match[1,3]
    if (is.na(end_row)) {
      r_in_range = start_row
    } else {
      r_in_range = paste0(start_row, ":", end_row)
    }
  } else {
    r_in_range = paste0("# keep in range '", in_str, "' not fully translated.")
  }

  r_in_range
}
