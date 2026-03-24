# Shared helpers for estimation commands and xi-prefixed estimation side effects

s2r_chr_vec_to_r = function(x) {
  restore.point("s2r_chr_vec_to_r")
  if (length(x) == 0) {
    return("character(0)")
  }
  paste0("c(", paste(vapply(x, quote_for_r_literal, character(1)), collapse = ", "), ")")
}

s2r_extract_xi_specs = function(model_terms) {
  restore.point("s2r_extract_xi_specs")
  specs = list()
  if (length(model_terms) == 0) return(specs)

  for (term in model_terms) {
    term = stringi::stri_trim_both(term)
    if (is.na(term) || term == "") next

    inter_match = stringi::stri_match_first_regex(term, "^i\\.([A-Za-z_][A-Za-z0-9_]*)\\*i\\.([A-Za-z_][A-Za-z0-9_]*)$")
    if (!is.na(inter_match[1,1])) {
      specs[[length(specs) + 1]] = list(var1 = inter_match[1,2], var2 = inter_match[1,3])
      next
    }

    single_match = stringi::stri_match_first_regex(term, "^i\\.([A-Za-z_][A-Za-z0-9_]*)$")
    if (!is.na(single_match[1,1])) {
      specs[[length(specs) + 1]] = list(var1 = single_match[1,2], var2 = NA_character_)
      next
    }
  }

  specs
}

s2r_model_term_to_vars = function(term) {
  restore.point("s2r_model_term_to_vars")
  if (is.na(term) || term == "") return(character(0))

  pieces = stringi::stri_split_regex(term, "#|\\*")[[1]]
  vars = character(0)

  for (piece in pieces) {
    piece = stringi::stri_trim_both(piece)
    if (piece == "") next

    while (TRUE) {
      m = stringi::stri_match_first_regex(piece, "^[A-Za-z0-9]+\\.(.+)$")
      if (is.na(m[1,1])) break
      piece = m[1,2]
    }

    if (stringi::stri_detect_regex(piece, "^[A-Za-z_][A-Za-z0-9_]*$")) {
      vars = c(vars, piece)
    }
  }

  unique(vars)
}

s2r_p_estimation_cmd = function(rest_of_cmd, estimator = NA_character_) {
  restore.point("s2r_p_estimation_cmd")

  parts = stringi::stri_match_first_regex(stringi::stri_trim_both(rest_of_cmd), "^\\s*(.*?)(?:,\\s*(.*))?$")
  main_part = stringi::stri_trim_both(parts[1,2])
  options_part = stringi::stri_trim_both(parts[1,3])

  parsed_if = s2r_parse_if_in(main_part)
  model_part = parsed_if$base_str

  tokens = stringi::stri_split_regex(stringi::stri_trim_both(model_part), "\\s+")[[1]]
  tokens = tokens[tokens != ""]

  dep_var = if (length(tokens) > 0) tokens[1] else NA_character_
  indep_terms = if (length(tokens) > 1) tokens[-1] else character(0)

  absorb_vars = character(0)
  cluster_vars = character(0)
  if (!is.na(options_part) && options_part != "") {
    absorb_match = stringi::stri_match_first_regex(options_part, "\\babsorb\\s*\\(([^)]+)\\)")
    if (!is.na(absorb_match[1,1])) {
      absorb_vars = stringi::stri_split_regex(stringi::stri_trim_both(absorb_match[1,2]), "\\s+")[[1]]
      absorb_vars = absorb_vars[absorb_vars != ""]
    }

    vce_match = stringi::stri_match_first_regex(options_part, "\\bvce\\s*\\(([^)]+)\\)")
    if (!is.na(vce_match[1,1])) {
      vce_opts = stringi::stri_split_regex(stringi::stri_trim_both(vce_match[1,2]), "\\s+")[[1]]
      vce_opts = vce_opts[vce_opts != ""]
      if (length(vce_opts) > 1 && vce_opts[1] == "cluster") {
        cluster_vars = vce_opts[-1]
      }
    }
  }

  xi_specs = s2r_extract_xi_specs(indep_terms)
  model_vars = unique(unlist(c(
    lapply(indep_terms, s2r_model_term_to_vars),
    lapply(absorb_vars, s2r_model_term_to_vars),
    lapply(cluster_vars, s2r_model_term_to_vars)
  )))

  list(
    estimator = estimator,
    dep_var = dep_var,
    indep_terms = indep_terms,
    model_vars = model_vars,
    absorb_vars = absorb_vars,
    xi_specs = xi_specs,
    if_str = parsed_if$if_str,
    options = options_part
  )
}

s2r_xi_specs_to_prefixes = function(xi_specs) {
  restore.point("s2r_xi_specs_to_prefixes")
  prefixes = character(0)
  if (length(xi_specs) == 0) return(prefixes)

  for (spec in xi_specs) {
    if (is.null(spec$var2) || is.na(spec$var2) || spec$var2 == "") {
      base_name = get_xi_base_name(spec$var1)
    } else {
      base_name = get_xi_interaction_basename(spec$var1, spec$var2)
    }
    prefixes = c(prefixes, paste0("_I", base_name, "_"))
  }

  unique(prefixes)
}

s2r_line_uses_xi_prefixes = function(line_text, xi_prefixes) {
  restore.point("s2r_line_uses_xi_prefixes")
  if (is.na(line_text) || line_text == "" || length(xi_prefixes) == 0) return(FALSE)

  if (dplyr::coalesce(stringi::stri_detect_fixed(line_text, "_I*"), FALSE)) {
    return(TRUE)
  }

  any(vapply(xi_prefixes, function(pref) {
    dplyr::coalesce(stringi::stri_detect_fixed(line_text, pref), FALSE)
  }, logical(1)))
}
