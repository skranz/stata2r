translate_stata_expression_with_r_values = function(stata_expr, line_num, cmd_df, context) {
  restore.point("translate_stata_expression_with_r_values")

  r_value_mappings = list()

  # We extract all e(...) and r(...) used in stata_expr
  e_matches = stringi::stri_extract_all_regex(stata_expr, "\\be\\([a-zA-Z0-9_]+\\)")[[1]]
  e_matches = e_matches[!is.na(e_matches)]
  for (m in e_matches) {
    inner = gsub("^[er]\\(|\\)$", "", m)
    r_value_mappings[[m]] = paste0("stata2r_env$e_", inner)
  }

  r_matches = stringi::stri_extract_all_regex(stata_expr, "\\br\\([a-zA-Z0-9_]+\\)")[[1]]
  r_matches = r_matches[!is.na(r_matches)]
  for (m in r_matches) {
    inner = gsub("^[er]\\(|\\)$", "", m)
    r_value_mappings[[m]] = paste0("stata2r_env$r_", inner)
  }

  translated_expr = translate_stata_expression_to_r(
    stata_expr,
    context = context,
    r_value_mappings = r_value_mappings
  )

  return(translated_expr)
}

