# Public helper and internal engine for identifying commands that matter for later data preparation

s2r_check_mod = function(stata_code) {
  restore.point("s2r_check_mod")

  if (length(stata_code) == 1 && is.character(stata_code) && dplyr::coalesce(stringi::stri_detect_fixed(stata_code, "\n"), FALSE)) {
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

s2r_check_mod_df = function(cmd_df) {
  restore.point("s2r_check_mod_df")

  if (NROW(cmd_df) == 0) {
    if (!("is_mod" %in% names(cmd_df))) cmd_df$is_mod = logical(0)
    if (!("do_translate" %in% names(cmd_df))) cmd_df$do_translate = logical(0)
    if (!("need_e_sample" %in% names(cmd_df))) cmd_df$need_e_sample = logical(0)
    if (!("need_xi" %in% names(cmd_df))) cmd_df$need_xi = logical(0)
    if (!("need_e_results" %in% names(cmd_df))) cmd_df$need_e_results = logical(0)
    if (!("need_r_results" %in% names(cmd_df))) cmd_df$need_r_results = logical(0)
    if (!("e_results_needed" %in% names(cmd_df))) cmd_df$e_results_needed = I(vector("list", 0))
    if (!("r_results_needed" %in% names(cmd_df))) cmd_df$r_results_needed = I(vector("list", 0))
    return(cmd_df)
  }

  if (!("is_xi_prefix" %in% names(cmd_df))) cmd_df$is_xi_prefix = rep(FALSE, NROW(cmd_df))
  if (!("e_results_needed" %in% names(cmd_df))) cmd_df$e_results_needed = I(replicate(nrow(cmd_df), character(0), simplify = FALSE))
  if (!("r_results_needed" %in% names(cmd_df))) cmd_df$r_results_needed = I(replicate(nrow(cmd_df), character(0), simplify = FALSE))

  cmd_df$is_mod = rep(FALSE, NROW(cmd_df))
  cmd_df$do_translate = rep(FALSE, NROW(cmd_df))
  cmd_df$need_e_sample = rep(FALSE, NROW(cmd_df))
  cmd_df$need_xi = rep(FALSE, NROW(cmd_df))
  cmd_df$need_e_results = rep(FALSE, NROW(cmd_df))
  cmd_df$need_r_results = rep(FALSE, NROW(cmd_df))

  # Commands that are inherently data-modifying are always kept.
  cmd_df$is_mod = cmd_df$stata_cmd %in% stata_data_manip_cmds

  active_needed_e_results = character(0)
  active_needed_r_results = character(0)

  for (i in NROW(cmd_df):1) {
    current_cmd = cmd_df$stata_cmd[i]
    rest_of_cmd = dplyr::coalesce(cmd_df$rest_of_cmd[i], "")

    used_e_macros = character(0)
    matches_e_used = stringi::stri_match_all_regex(rest_of_cmd, "e\\(([^)]+)\\)")[[1]]
    if (NROW(matches_e_used) > 0) {
      used_e_macros = unique(paste0("e(", matches_e_used[,2], ")"))
    }

    used_r_macros = character(0)
    matches_r_used = stringi::stri_match_all_regex(rest_of_cmd, "r\\(([^)]+)\\)")[[1]]
    if (NROW(matches_r_used) > 0) {
      used_r_macros = unique(paste0("r(", matches_r_used[,2], ")"))
    }

    if (current_cmd %in% stata_estimation_cmds) {
      potential_e_results_produced = c("e(sample)", "e(N)", "e(r2)", "e(df_r)", "e(rmse)", "e(b)", "e(V)")
      if (any(potential_e_results_produced %in% active_needed_e_results)) {
        cmd_df$is_mod[i] = TRUE
        cmd_df$e_results_needed[[i]] = union(cmd_df$e_results_needed[[i]], intersect(potential_e_results_produced, active_needed_e_results))
        active_needed_e_results = setdiff(active_needed_e_results, potential_e_results_produced)
      }
    }

    if (current_cmd %in% stata_r_result_cmds) {
      potential_r_results_produced = c("r(N)", "r(mean)", "r(sd)", "r(min)", "r(max)", "r(sum)", "r(p50)")
      if (any(potential_r_results_produced %in% active_needed_r_results)) {
        cmd_df$is_mod[i] = TRUE
        cmd_df$r_results_needed[[i]] = union(cmd_df$r_results_needed[[i]], intersect(potential_r_results_produced, active_needed_r_results))
        active_needed_r_results = setdiff(active_needed_r_results, potential_r_results_produced)
      }
    }

    active_needed_e_results = union(active_needed_e_results, used_e_macros)
    active_needed_r_results = union(active_needed_r_results, used_r_macros)
  }

  # Xi side effects from xi-prefixed estimation commands are only needed if later code uses the generated _I variables.
  for (i in seq_len(NROW(cmd_df))) {
    if (isTRUE(cmd_df$is_xi_prefix[i]) && cmd_df$stata_cmd[i] %in% stata_estimation_cmds) {
      parsed_est = s2r_p_estimation_cmd(cmd_df$rest_of_cmd[i], estimator = cmd_df$stata_cmd[i])
      xi_prefixes = s2r_xi_specs_to_prefixes(parsed_est$xi_specs)

      need_xi_i = FALSE
      if (length(xi_prefixes) > 0 && i < NROW(cmd_df)) {
        later_lines = cmd_df$do_code[(i + 1):NROW(cmd_df)]
        need_xi_i = any(vapply(later_lines, s2r_line_uses_xi_prefixes, logical(1), xi_prefixes = xi_prefixes))
      }

      cmd_df$need_xi[i] = need_xi_i
      if (need_xi_i) {
        cmd_df$is_mod[i] = TRUE
      }
    }
  }

  cmd_df$need_e_sample = vapply(cmd_df$e_results_needed, function(x) "e(sample)" %in% x, logical(1))
  cmd_df$need_e_results = lengths(cmd_df$e_results_needed) > 0
  cmd_df$need_r_results = lengths(cmd_df$r_results_needed) > 0

  # Final explicit overrides
  for (k in seq_len(NROW(cmd_df))) {
    if (cmd_df$stata_cmd[k] %in% stata_non_data_manip_cmds) {
      keep_due_to_results = isTRUE(cmd_df$need_e_results[k]) || isTRUE(cmd_df$need_r_results[k]) || isTRUE(cmd_df$need_xi[k])
      if (!keep_due_to_results) {
        cmd_df$is_mod[k] = FALSE
      }
    }
  }

  cmd_df$is_mod[is.na(cmd_df$stata_cmd)] = FALSE

  if ("clear" %in% cmd_df$stata_cmd) {
    is_standalone_clear = cmd_df$stata_cmd == "clear" & (is.na(cmd_df$rest_of_cmd) | cmd_df$rest_of_cmd == "")
    cmd_df$is_mod[is_standalone_clear] = TRUE
  }

  cmd_df$do_translate = cmd_df$is_mod
  return(cmd_df)
}
