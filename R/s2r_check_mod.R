# Public helper and internal engine for identifying commands that matter for later data preparation

s2r_check_mod = function(stata_code) {
  restore.point("s2r_check_mod")

  if (length(stata_code) == 1 && is.character(stata_code) && fast_coalesce(stringi::stri_detect_fixed(stata_code, "\n"), FALSE)) {
    stata_code = stringi::stri_split_fixed(stata_code, "\n")[[1]]
  }
  if (is.list(stata_code)) {
    stata_code = unlist(stata_code)
  }
  if (!is.character(stata_code)) {
    stata_code = as.character(stata_code)
  }

  cmd_df = do_parse(stata_code)
  s2r_check_mod_df(cmd_df)
}

s2r_tabulate_has_gen_option = function(rest_of_cmd) {
  if (is.na(rest_of_cmd) || rest_of_cmd == "") return(FALSE)

  parts = stringi::stri_match_first_regex(
    rest_of_cmd,
    "^\\s*[^,]*?(?:,\\s*(.*))?$"
  )

  options_str = stringi::stri_trim_both(parts[1, 2])
  if (is.na(options_str) || options_str == "") return(FALSE)

  fast_coalesce(
    stringi::stri_detect_regex(options_str, "\\b(?:gen|generate)\\s*\\("),
    FALSE
  )
}

s2r_check_mod_df = function(cmd_df) {
  restore.point("s2r_check_mod_df")

  n_rows = NROW(cmd_df)
  if (n_rows == 0) {
    if (!("is_mod" %in% names(cmd_df))) cmd_df$is_mod = logical(0)
    if (!("do_translate" %in% names(cmd_df))) cmd_df$do_translate = logical(0)
    if (!("need_xi" %in% names(cmd_df))) cmd_df$need_xi = logical(0)
    return(cmd_df)
  }

  if (!("is_xi_prefix" %in% names(cmd_df))) cmd_df$is_xi_prefix = rep(FALSE, n_rows)

  cmd_df$is_mod = rep(FALSE, n_rows)
  cmd_df$do_translate = rep(FALSE, n_rows)
  cmd_df$need_xi = rep(FALSE, n_rows)

  # Commands that are inherently data-modifying are always kept.
  cmd_df$is_mod = !is.na(cmd_df$stata_cmd) & (cmd_df$stata_cmd %in% stata_data_manip_cmds)

  # tabulate is usually display-only, but tabulate ..., gen(...) creates dummy variables.
  idx_tabulate_gen = which(!is.na(cmd_df$stata_cmd) & cmd_df$stata_cmd %in% c("tabulate", "tab"))
  if (length(idx_tabulate_gen) > 0) {
    has_gen = vapply(
      cmd_df$rest_of_cmd[idx_tabulate_gen],
      s2r_tabulate_has_gen_option,
      logical(1)
    )
    cmd_df$is_mod[idx_tabulate_gen[has_gen]] = TRUE
  }

  cmd_is_est = !is.na(cmd_df$stata_cmd) & (cmd_df$stata_cmd %in% stata_estimation_cmds)
  rest_of_cmd_vec = cmd_df$rest_of_cmd
  rest_of_cmd_vec[is.na(rest_of_cmd_vec)] = ""

  # Xi side effects from xi-prefixed estimation commands
  idx_xi_est = which(cmd_df$is_xi_prefix & cmd_is_est)
  if (length(idx_xi_est) > 0) {
    do_code_vec = cmd_df$do_code
    do_code_vec[is.na(do_code_vec)] = ""

    for (i in idx_xi_est) {
      parsed_est = s2r_p_estimation_cmd(rest_of_cmd_vec[i], estimator = cmd_df$stata_cmd[i])
      xi_prefixes = s2r_xi_specs_to_prefixes(parsed_est$xi_specs)

      need_xi_i = FALSE
      if (length(xi_prefixes) > 0 && i < n_rows) {
        later_lines = do_code_vec[(i + 1):n_rows]

        # Fast vectorized check across all later lines at once
        if (any(stringi::stri_detect_fixed(later_lines, "_I*"))) {
          need_xi_i = TRUE
        } else {
          for (pref in xi_prefixes) {
            if (any(stringi::stri_detect_fixed(later_lines, pref))) {
              need_xi_i = TRUE
              break
            }
          }
        }
      }

      cmd_df$need_xi[i] = need_xi_i
      if (need_xi_i) {
        cmd_df$is_mod[i] = TRUE
      }
    }
  }

  # Final explicit overrides (vectorized)
  is_non_manip = !is.na(cmd_df$stata_cmd) & (cmd_df$stata_cmd %in% stata_non_data_manip_cmds)

  override_idx = is_non_manip & !cmd_df$need_xi
  if (any(override_idx)) {
    cmd_df$is_mod[override_idx] = FALSE
  }

  cmd_df$is_mod[is.na(cmd_df$stata_cmd)] = FALSE

  is_standalone_clear = !is.na(cmd_df$stata_cmd) & cmd_df$stata_cmd == "clear" & (rest_of_cmd_vec == "")
  if (any(is_standalone_clear)) {
    cmd_df$is_mod[is_standalone_clear] = TRUE
  }

  cmd_df$do_translate = cmd_df$is_mod
  return(cmd_df)
}
