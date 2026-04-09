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

s2r_check_mod_df = function(cmd_df) {
  restore.point("s2r_check_mod_df")

  n_rows = NROW(cmd_df)
  if (n_rows == 0) {
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

  if (!("is_xi_prefix" %in% names(cmd_df))) cmd_df$is_xi_prefix = rep(FALSE, n_rows)

  cmd_df$is_mod = rep(FALSE, n_rows)
  cmd_df$do_translate = rep(FALSE, n_rows)
  cmd_df$need_e_sample = rep(FALSE, n_rows)
  cmd_df$need_xi = rep(FALSE, n_rows)
  cmd_df$need_e_results = rep(FALSE, n_rows)
  cmd_df$need_r_results = rep(FALSE, n_rows)

  # Commands that are inherently data-modifying are always kept.
  cmd_df$is_mod = !is.na(cmd_df$stata_cmd) & (cmd_df$stata_cmd %in% stata_data_manip_cmds)

  # Prepare string vector to avoid slow fast_coalesce inside the loop
  rest_of_cmd_vec = cmd_df$rest_of_cmd
  rest_of_cmd_vec[is.na(rest_of_cmd_vec)] = ""

  # Vectorized extraction of all e() and r() macros at once
  e_matches_list = stringi::stri_match_all_regex(rest_of_cmd_vec, "e\\(([^)]+)\\)")
  r_matches_list = stringi::stri_match_all_regex(rest_of_cmd_vec, "r\\(([^)]+)\\)")

  # Precompute logical vectors for fast lookup
  cmd_is_est = !is.na(cmd_df$stata_cmd) & (cmd_df$stata_cmd %in% stata_estimation_cmds)
  cmd_is_r_res = !is.na(cmd_df$stata_cmd) & (cmd_df$stata_cmd %in% stata_r_result_cmds)

  potential_e_results_produced = c("e(sample)", "e(N)", "e(r2)", "e(df_r)", "e(rmse)", "e(b)", "e(V)")
  potential_r_results_produced = c("r(N)", "r(mean)", "r(sd)", "r(min)", "r(max)", "r(sum)", "r(p50)")

  e_results_needed_list = vector("list", n_rows)
  r_results_needed_list = vector("list", n_rows)

  active_needed_e_results = character(0)
  active_needed_r_results = character(0)

  # Backward pass to resolve macro dependencies
  for (i in n_rows:1) {
    # Check if current command fulfills needed macros
    if (cmd_is_est[i]) {
      intersect_e = intersect(potential_e_results_produced, active_needed_e_results)
      if (length(intersect_e) > 0) {
        cmd_df$is_mod[i] = TRUE
        e_results_needed_list[[i]] = intersect_e
        active_needed_e_results = setdiff(active_needed_e_results, potential_e_results_produced)
      }
    }

    if (cmd_is_r_res[i]) {
      intersect_r = intersect(potential_r_results_produced, active_needed_r_results)
      if (length(intersect_r) > 0) {
        cmd_df$is_mod[i] = TRUE
        r_results_needed_list[[i]] = intersect_r
        active_needed_r_results = setdiff(active_needed_r_results, potential_r_results_produced)
      }
    }

    # Add new requirements from this command
    mat_e = e_matches_list[[i]]
    if (!is.na(mat_e[1,1])) {
      used_e_macros = unique(paste0("e(", mat_e[,2], ")"))
      active_needed_e_results = union(active_needed_e_results, used_e_macros)
    }

    mat_r = r_matches_list[[i]]
    if (!is.na(mat_r[1,1])) {
      used_r_macros = unique(paste0("r(", mat_r[,2], ")"))
      active_needed_r_results = union(active_needed_r_results, used_r_macros)
    }
  }

  cmd_df$e_results_needed = I(e_results_needed_list)
  cmd_df$r_results_needed = I(r_results_needed_list)

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

  cmd_df$need_e_sample = vapply(e_results_needed_list, function(x) "e(sample)" %in% x, logical(1))
  cmd_df$need_e_results = lengths(e_results_needed_list) > 0
  cmd_df$need_r_results = lengths(r_results_needed_list) > 0

  # Final explicit overrides (vectorized)
  is_non_manip = !is.na(cmd_df$stata_cmd) & (cmd_df$stata_cmd %in% stata_non_data_manip_cmds)
  keep_due_to_results = cmd_df$need_e_results | cmd_df$need_r_results | cmd_df$need_xi

  override_idx = is_non_manip & !keep_due_to_results
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
