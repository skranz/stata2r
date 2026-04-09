# FILE: R/t_merge.R

# 1. Parsing Phase
s2r_p_merge = function(rest_of_cmd) {
  restore.point("s2r_p_merge")
  match = stringi::stri_match_first_regex(stringi::stri_trim_both(rest_of_cmd), "^\\s*([1m]:[1m])\\s+(.*?)\\s+using\\s+(\"[^\"]+\"|`[^']+'|[^,\\s]+)(?:,\\s*(.*))?$")

  if (is.na(match[1,1])) {
    match_old = stringi::stri_match_first_regex(stringi::stri_trim_both(rest_of_cmd), "^\\s*(.*?)\\s+using\\s+(\"[^\"]+\"|`[^']+'|[^,\\s]+)(?:,\\s*(.*))?$")
    if (is.na(match_old[1,1])) return(list(merge_type = NA_character_))
    return(list(merge_type = "1:1", varlist = stringi::stri_trim_both(match_old[1,2]), file = stringi::stri_trim_both(match_old[1,3]), options = stringi::stri_trim_both(match_old[1,4])))
  }

  list(merge_type = match[1,2], varlist = stringi::stri_trim_both(match[1,3]), file = stringi::stri_trim_both(match[1,4]), options = stringi::stri_trim_both(match[1,5]))
}

# 2. Code Generation Phase
t_merge = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_merge")
  parsed = s2r_p_merge(rest_of_cmd)
  if (is.na(parsed$merge_type)) return(paste0("# Failed to parse merge: ", rest_of_cmd))

  file_r_expr = resolve_stata_filename(parsed$file, cmd_df, line_num, default_base_dir_var = "working_dir")

  has_nogenerate = fast_coalesce(stringi::stri_detect_regex(parsed$options, "\\bno(?:generate|gen)\\b"), FALSE)
  keep_opt = NA_character_
  if (!is.na(parsed$options)) {
    k_match = stringi::stri_match_first_regex(parsed$options, "\\bkeep\\s*\\(([^)]+)\\)")
    if (!is.na(k_match[1,1])) keep_opt = stringi::stri_trim_both(k_match[1,2])
  }

  args = c("data = data", paste0("merge_type = ", quote_for_r_literal(parsed$merge_type)),
           paste0("varlist = ", quote_for_r_literal(parsed$varlist)), paste0("file_path = ", file_r_expr),
           paste0("keep_opt = ", quote_for_r_literal(keep_opt)), paste0("has_nogenerate = ", has_nogenerate))

  r_code = paste0("data = scmd_merge(", paste(args, collapse = ", "), ")")
  r_code = paste0(r_code, "\nif (isTRUE(stata2r_env$has_original_order_idx)) { data = dplyr::mutate(data, stata2r_original_order_idx = dplyr::row_number()) }")

  return(r_code)
}

# 3. Runtime Execution Phase
scmd_merge = function(data, merge_type, varlist, file_path, keep_opt = NA_character_, has_nogenerate = FALSE) {
  restore.point("scmd_merge")
  merge_keys = expand_varlist(varlist, names(data))

  using_data = haven::read_dta(file_path)
  data = sfun_normalize_string_nas(sfun_strip_stata_attributes(data))
  using_data = sfun_normalize_string_nas(sfun_strip_stata_attributes(using_data))

  for (k in merge_keys) {
    data[[k]] = as.numeric(data[[k]])
    using_data[[k]] = as.numeric(using_data[[k]])
  }

  if (merge_type == "1:1") {
    if (any(duplicated(data[, merge_keys, drop=FALSE]))) stop("Merge 1:1 failed: Duplicate keys in master.")
    if (any(duplicated(using_data[, merge_keys, drop=FALSE]))) stop("Merge 1:1 failed: Duplicate keys in using.")
  }

  common_not_by = setdiff(intersect(names(data), names(using_data)), merge_keys)
  if (length(common_not_by) > 0) using_data = using_data[, !names(using_data) %in% common_not_by, drop=FALSE]

  join_func = dplyr::left_join
  if (merge_type == "m:m" || (!is.na(keep_opt) && grepl("\\ball\\b", keep_opt)) || (merge_type == "1:1" && has_nogenerate && is.na(keep_opt))) join_func = dplyr::full_join
  else if (!is.na(keep_opt) && grepl("\\bmatch\\b", keep_opt)) join_func = dplyr::inner_join
  else if (!is.na(keep_opt) && grepl("\\busing\\b", keep_opt)) join_func = dplyr::right_join

  # Track origins for Stata's _merge indicator
  if (!has_nogenerate) {
    data$.stata_in_master = 1L
    using_data$.stata_in_using = 1L
  }

  data = join_func(data, using_data, by = merge_keys)
  data = sfun_normalize_string_nas(data)

  if (!has_nogenerate) {
    data$`_merge` = dplyr::case_when(
      !is.na(data$.stata_in_master) & is.na(data$.stata_in_using) ~ 1L,
      is.na(data$.stata_in_master) & !is.na(data$.stata_in_using) ~ 2L,
      !is.na(data$.stata_in_master) & !is.na(data$.stata_in_using) ~ 3L,
      TRUE ~ NA_integer_
    )
    # Clean up tracking cols
    data$.stata_in_master = NULL
    data$.stata_in_using = NULL
  }

  return(data)
}
