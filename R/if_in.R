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

#' Normalize translated condition strings for runtime evaluation
#'
#' Translated expressions may contain dplyr helpers such as
#' dplyr::row_number() or dplyr::n(), which are valid inside mutate/filter
#' but not when evaluated directly via eval(). For runtime if/in evaluation
#' we replace them by explicit helper symbols backed by concrete vectors/scalars.
#'
#' @param r_cond The translated R condition string
#' @return A condition string that can be safely evaluated with eval()
s2r_prepare_runtime_cond = function(r_cond) {
  restore.point("s2r_prepare_runtime_cond")
  if (is.na(r_cond) || r_cond == "") return(r_cond)

  out = r_cond

  # Main translations produced by translate_stata_expression_to_r()
  out = stringi::stri_replace_all_fixed(out, "dplyr::row_number()", ".stata_row_index")
  out = stringi::stri_replace_all_fixed(out, "dplyr::n()", ".stata_n_rows")

  # Defensive fallback in case untranslated special tokens survive
  out = stringi::stri_replace_all_regex(out, "\\b_n\\b", ".stata_row_index")
  out = stringi::stri_replace_all_regex(out, "\\b_N\\b", ".stata_n_rows")

  return(out)
}

#' Evaluate a Stata condition string at runtime, returning a boolean vector
s2r_eval_cond = function(data, r_cond, envir = parent.frame(),
                         stata_row_index = NULL,
                         stata_n_rows = NULL) {
  restore.point("s2r_eval_cond")

  if (is.na(r_cond) || r_cond == "") return(rep(TRUE, NROW(data)))

  r_cond = s2r_prepare_runtime_cond(r_cond)

  expr = base::parse(text = r_cond)
  used_vars = all.vars(expr)
  data_cols = names(data)

  n = NROW(data)

  if (is.null(stata_row_index)) {
    stata_row_index = seq_len(n)
  }
  if (is.null(stata_n_rows)) {
    stata_n_rows = n
  }

  eval_list = list(
    ".stata_row_index" = stata_row_index,
    ".stata_n_rows" = stata_n_rows,
    "NA_real_" = Inf,
    "NA_integer_" = Inf,
    "NA_character_" = "",
    "NA" = Inf,
    ".data" = data,
    "sfun_missing" = function(...) {
      args = list(...)
      if (length(args) == 0) return(rep(FALSE, n))

      check_missing = function(x) {
        if (is.numeric(x)) {
          return(is.na(x) | is.infinite(x))
        } else if (is.character(x)) {
          return(is.na(x) | stringi::stri_trim_both(x) == "")
        }
        return(is.na(x))
      }

      res = check_missing(args[[1]])
      if (length(args) > 1) {
        for (i in 2:length(args)) {
          res = res | check_missing(args[[i]])
        }
      }
      return(res)
    }
  )

  reserved = c(s2r_eval_reserved_words(), ".stata_row_index", ".stata_n_rows")

  for (v in used_vars) {
    if (v %in% reserved) next

    if (v %in% data_cols) {
      eval_list[[v]] = to_stata_na(data[[v]])
      next
    }

    mcols = data_cols[startsWith(data_cols, v)]
    if (length(mcols) == 1) {
      eval_list[[v]] = to_stata_na(data[[mcols[1]]])
    } else if (length(mcols) > 1) {
      stop(paste("Ambiguous abbreviation in condition:", v))
    }
  }

  cond_val = base::eval(expr, envir = eval_list, enclos = envir)
  return(s2r_stata_logical(cond_val))
}

#' Evaluate a Stata condition string within by-groups
#'
#' This is needed for commands like:
#'   by id: keep if _n == 1
#'   by id: drop if _n == _N
#'
#' The important semantic point is that Stata evaluates _n and _N inside
#' the current by-group. This implementation computes group-wise _n and _N
#' vectors once and then evaluates the condition once over the full data.
s2r_eval_cond_by_group = function(data, r_cond, group_vars = character(0), envir = parent.frame()) {
  restore.point("s2r_eval_cond_by_group")

  if (is.na(r_cond) || r_cond == "") {
    return(rep(TRUE, NROW(data)))
  }

  n = NROW(data)
  if (n == 0) return(logical(0))

  group_vars = group_vars[!is.na(group_vars) & group_vars != ""]
  group_vars = expand_varlist(paste(group_vars, collapse = " "), names(data))

  if (length(group_vars) == 0) {
    return(s2r_eval_cond(data, r_cond, envir = envir))
  }

  # Normalize the condition for fast paths.
  cond_norm = stringi::stri_replace_all_regex(r_cond, "\\s+", "")
  cond_norm = s2r_prepare_runtime_cond(cond_norm)

  group_list = lapply(group_vars, function(v) {
    x = data[[v]]
    x = as.character(x)
    x[is.na(x)] = "<STATA2R_NA_GROUP_VALUE>"
    x
  })
  names(group_list) = group_vars

  # Common Stata idioms. These avoid constructing grouped _n/_N vectors.
  # They preserve the current row order, which is exactly what Stata's by:
  # prefix uses after the preceding sort.
  group_df = data.frame(group_list, check.names = FALSE, stringsAsFactors = FALSE)

  if (cond_norm %in% c(".stata_row_index==1", "1==.stata_row_index")) {
    return(!duplicated(group_df))
  }

  if (cond_norm %in% c(".stata_row_index==.stata_n_rows", ".stata_n_rows==.stata_row_index")) {
    return(!duplicated(group_df, fromLast = TRUE))
  }

  group_fac = do.call(
    interaction,
    c(group_list, list(drop = TRUE, lex.order = FALSE))
  )

  stata_row_index = ave(seq_len(n), group_fac, FUN = seq_along)
  stata_n_rows = ave(rep.int(1L, n), group_fac, FUN = length)

  s2r_eval_cond(
    data = data,
    r_cond = r_cond,
    envir = envir,
    stata_row_index = stata_row_index,
    stata_n_rows = stata_n_rows
  )
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
#' @param envir The enclosing environment for evaluation
#' @return The subsetted dataframe
s2r_eval_if_in = function(data, r_if_cond = NA_character_, r_in_range = NA_character_, envir = parent.frame()) {
  restore.point("seval_if_in")

  if (!is.na(r_if_cond) && r_if_cond != "") {
    cond_val = s2r_eval_cond(data, r_if_cond, envir = envir)
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
