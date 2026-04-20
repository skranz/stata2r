# FILE: R/stata_xi.R

#' Helper to generate xi base name
get_xi_base_name = function(varname) {
  if (base::endsWith(varname, "_factor")) {
    return(stringi::stri_replace_last_fixed(varname, "_factor", "_f"))
  } else if (varname == "region_cat") {
    return("region_ca")
  }
  return(varname)
}

#' Helper to generate xi interaction base name
get_xi_interaction_basename = function(var1, var2) {
  short_var1 = stringi::stri_sub(var1, 1, min(stringi::stri_length(var1), 3))
  short_var2 = stringi::stri_sub(var2, 1, min(stringi::stri_length(var2), 3))
  return(paste0(short_var1, "X", short_var2))
}

#' Integer-ish test with a small tolerance, useful for haven float columns
s2r_xi_is_integerish = function(x, tol = 1e-8) {
  if (length(x) == 0) return(logical(0))
  !is.na(x) & abs(x - round(x)) <= tol
}

#' Prepare a Stata xi categorical variable
#'
#' Returns a list with:
#' - kind: "numeric" or "string"
#' - values: the comparable runtime values
#' - levels: sorted unique nonmissing category values
#' - suffixes: xi suffixes corresponding to each level
s2r_xi_prepare_var = function(x, varname) {
  if (inherits(x, "haven_labelled")) {
    x = haven::zap_labels(x)
  }

  if (is.factor(x)) {
    x = as.character(x)
  }

  if (is.character(x)) {
    values = as.character(x)
    levels = unique(values[!is.na(values)])
    if (length(levels) > 0) {
      levels = stringi::stri_sort(levels, locale = "C")
    }
    suffixes = as.character(seq_along(levels))

    return(list(
      kind = "string",
      values = values,
      levels = levels,
      suffixes = suffixes
    ))
  }

  if (is.logical(x)) {
    x = as.numeric(x)
  }

  if (!is.numeric(x)) {
    stop(
      paste0(
        "scmd_xi: variable '", varname,
        "' must be numeric, string, labelled numeric, factor, or logical."
      )
    )
  }

  values = as.numeric(x)
  levels = sort(unique(values[!is.na(values)]))

  if (all(s2r_xi_is_integerish(levels))) {
    suffixes = as.character(as.integer(round(levels)))
  } else {
    # For non-integer numeric categories, Stata's exact naming rule is not
    # documented as clearly as the integer and string cases. A stable,
    # Stata-like choice is sequential coding in sorted order.
    suffixes = as.character(seq_along(levels) - 1L)
  }

  list(
    kind = "numeric",
    values = values,
    levels = levels,
    suffixes = suffixes
  )
}

#' Keep all levels when noomit is requested; otherwise drop the first/base level
s2r_xi_included_levels = function(prep, noomit = FALSE) {
  if (length(prep$levels) == 0) {
    return(list(levels = prep$levels, suffixes = prep$suffixes))
  }

  if (isTRUE(noomit)) {
    return(list(levels = prep$levels, suffixes = prep$suffixes))
  }

  if (length(prep$levels) == 1) {
    return(list(
      levels = prep$levels[0],
      suffixes = prep$suffixes[0]
    ))
  }

  list(
    levels = prep$levels[-1],
    suffixes = prep$suffixes[-1]
  )
}

#' Human-readable label text for xi-generated dummy labels
s2r_xi_level_label = function(level_value, kind) {
  if (kind == "string") {
    return(level_value)
  }

  if (is.na(level_value)) {
    return(".")
  }

  if (s2r_xi_is_integerish(level_value)) {
    return(as.character(as.integer(round(level_value))))
  }

  format(
    signif(level_value, digits = 12),
    scientific = FALSE,
    trim = TRUE
  )
}

#' Vectorized equality check for prepared xi values
s2r_xi_match_level = function(values, level, kind) {
  if (kind == "string") {
    return(!is.na(values) & values == level)
  }
  !is.na(values) & values == level
}

#' Generate dummy variables for a single categorical variable
sfun_xi_create_dummies = function(data, varname, noomit = FALSE) {
  prep = s2r_xi_prepare_var(data[[varname]], varname)
  included = s2r_xi_included_levels(prep, noomit = noomit)

  if (length(included$levels) == 0) {
    return(data)
  }

  base_name = get_xi_base_name(varname)

  for (i in seq_along(included$levels)) {
    lvl = included$levels[i]
    suffix = included$suffixes[i]
    new_col = paste0("_I", base_name, "_", suffix)

    is_match = s2r_xi_match_level(prep$values, lvl, prep$kind)
    is_valid_non_match = !is.na(prep$values) & !is_match

    data[[new_col]] = dplyr::if_else(
      is_match,
      1L,
      dplyr::if_else(is_valid_non_match, 0L, NA_integer_)
    )

    attr(data[[new_col]], "label") = paste0(
      varname, "==", s2r_xi_level_label(lvl, prep$kind)
    )
  }

  data
}

#' Generate xi interaction dummies for two categorical variables
sfun_xi_create_interaction_dummies = function(data, var1, var2, noomit = FALSE) {
  prep1 = s2r_xi_prepare_var(data[[var1]], var1)
  prep2 = s2r_xi_prepare_var(data[[var2]], var2)

  inc1 = s2r_xi_included_levels(prep1, noomit = noomit)
  inc2 = s2r_xi_included_levels(prep2, noomit = noomit)

  if (length(inc1$levels) == 0 || length(inc2$levels) == 0) {
    return(data)
  }

  base = get_xi_interaction_basename(var1, var2)

  for (i in seq_along(inc1$levels)) {
    lvl1 = inc1$levels[i]
    suff1 = inc1$suffixes[i]

    match1 = s2r_xi_match_level(prep1$values, lvl1, prep1$kind)

    for (j in seq_along(inc2$levels)) {
      lvl2 = inc2$levels[j]
      suff2 = inc2$suffixes[j]

      match2 = s2r_xi_match_level(prep2$values, lvl2, prep2$kind)
      new_col = paste0("_I", base, "_", suff1, "_", suff2)

      is_match = match1 & match2
      is_valid_non_match = !is.na(prep1$values) & !is.na(prep2$values) & !is_match

      data[[new_col]] = dplyr::if_else(
        is_match,
        1L,
        dplyr::if_else(is_valid_non_match, 0L, NA_integer_)
      )

      attr(data[[new_col]], "label") = paste0(
        var1, "==", s2r_xi_level_label(lvl1, prep1$kind),
        " & ",
        var2, "==", s2r_xi_level_label(lvl2, prep2$kind)
      )
    }
  }

  data
}

#' Runtime execution for Stata xi command
scmd_xi = function(data, var1, var2 = NA_character_, noomit = FALSE) {
  restore.point("scmd_xi")

  data = sfun_xi_create_dummies(data, var1, noomit = noomit)

  if (!is.na(var2) && var2 != "") {
    data = sfun_xi_create_dummies(data, var2, noomit = noomit)
    data = sfun_xi_create_interaction_dummies(data, var1, var2, noomit = noomit)
  }

  data
}
