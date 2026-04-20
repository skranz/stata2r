# FILE: R/t_xi.R

# 1. Parsing Phase
s2r_p_xi = function(rest_of_cmd) {
  restore.point("s2r_p_xi")

  parts = stringi::stri_match_first_regex(
    stringi::stri_trim_both(rest_of_cmd),
    "^\\s*(.*?)(?:,\\s*(.*))?$"
  )

  specs_part = stringi::stri_trim_both(parts[1, 2])
  options = stringi::stri_trim_both(parts[1, 3])

  if (is.na(specs_part) || specs_part == "") {
    return(list(specs = list(), noomit = FALSE, options = options))
  }

  spec_tokens = stringi::stri_split_regex(specs_part, "\\s+")[[1]]
  spec_tokens = spec_tokens[!is.na(spec_tokens) & spec_tokens != ""]

  specs = s2r_extract_xi_specs(spec_tokens)

  noomit = FALSE
  if (!is.na(options) && options != "") {
    noomit = fast_coalesce(
      stringi::stri_detect_regex(options, "(^|[[:space:],])noomit($|[[:space:],])"),
      FALSE
    )
  }

  list(
    specs = specs,
    noomit = noomit,
    options = options
  )
}

# 2. Code Generation Phase
t_xi = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_xi")

  parsed = s2r_p_xi(rest_of_cmd)

  if (length(parsed$specs) == 0) {
    return(paste0("# xi command: Unsupported syntax for: ", rest_of_cmd))
  }

  code_lines = character(0)

  for (spec in parsed$specs) {
    args = c(
      "data = data",
      paste0("var1 = ", quote_for_r_literal(spec$var1)),
      paste0("noomit = ", parsed$noomit)
    )

    if (!is.null(spec$var2) && !is.na(spec$var2) && spec$var2 != "") {
      args = c(args, paste0("var2 = ", quote_for_r_literal(spec$var2)))
    }

    code_lines = c(
      code_lines,
      paste0("data = scmd_xi(", paste(args, collapse = ", "), ")")
    )
  }

  handled_options = character(0)
  if (parsed$noomit) {
    handled_options = c(handled_options, "noomit")
  }

  if (!is.na(parsed$options) && parsed$options != "") {
    option_tokens = stringi::stri_split_regex(parsed$options, "[[:space:]]+")[[1]]
    option_tokens = option_tokens[!is.na(option_tokens) & option_tokens != ""]
    unhandled = setdiff(option_tokens, handled_options)
    if (length(unhandled) > 0) {
      code_lines = c(
        code_lines,
        paste0("# xi options currently ignored: ", paste(unhandled, collapse = " "))
      )
    }
  }

  paste(code_lines, collapse = "\n")
}
