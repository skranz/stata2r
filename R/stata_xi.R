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

#' Generate dummy variables for a single categorical variable
sfun_xi_create_dummies = function(data, varname) {
  col = haven::zap_labels(data[[varname]])
  u_vals = sort(unique(col[!is.na(col)]))

  if (length(u_vals) > 0) {
    base_level = u_vals[1]
    levels_to_dummy = setdiff(u_vals, base_level)

    for (lvl in levels_to_dummy) {
      new_col = paste0("_I", get_xi_base_name(varname), "_", lvl)
      data[[new_col]] = dplyr::if_else(!is.na(col) & col == lvl, 1L, dplyr::if_else(!is.na(col) & col != lvl, 0L, NA_integer_))
      attr(data[[new_col]], "label") = paste0(varname, "==", lvl)
    }
  }
  return(data)
}

#' Runtime execution for Stata xi command
scmd_xi = function(data, var1, var2 = NA_character_) {
  restore.point("scmd_xi")

  data = sfun_xi_create_dummies(data, var1)

  if (!is.na(var2)) {
    data = sfun_xi_create_dummies(data, var2)

    col1 = haven::zap_labels(data[[var1]])
    col2 = haven::zap_labels(data[[var2]])

    u_vals1 = sort(unique(col1[!is.na(col1)]))
    u_vals2 = sort(unique(col2[!is.na(col2)]))

    if (length(u_vals1) > 0 && length(u_vals2) > 0) {
      levels1 = setdiff(u_vals1, u_vals1[1])
      levels2 = setdiff(u_vals2, u_vals2[1])

      base = get_xi_interaction_basename(var1, var2)

      for (lvl1 in levels1) {
        for (lvl2 in levels2) {
          new_col = paste0("_I", base, "_", lvl1, "_", lvl2)
          is_match = !is.na(col1) & !is.na(col2) & col1 == lvl1 & col2 == lvl2
          is_valid_non_match = !is.na(col1) & !is.na(col2) & (col1 != lvl1 | col2 != lvl2)

          data[[new_col]] = dplyr::if_else(is_match, 1L, dplyr::if_else(is_valid_non_match, 0L, NA_integer_))
          attr(data[[new_col]], "label") = paste0(var1, "==", lvl1, " & ", var2, "==", lvl2)
        }
      }
    }
  }

  return(data)
}
