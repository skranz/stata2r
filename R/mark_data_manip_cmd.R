mark_data_manip_cmd = function(cmd_df) {
  restore.point("mark_data_manip_cmd")

  if (NROW(cmd_df) == 0) {
    cmd_df$do_translate = logical(0)
    # Ensure e_results_needed and r_results_needed exist if cmd_df is empty but structured
    if (!("e_results_needed" %in% names(cmd_df))) cmd_df$e_results_needed = I(vector("list", 0))
    if (!("r_results_needed" %in% names(cmd_df))) cmd_df$r_results_needed = I(vector("list", 0))
    return(cmd_df)
  }

  cmd_df$do_translate = rep(FALSE, NROW(cmd_df))
  # Ensure list columns are initialized if not present (e.g. from older cmd_df structure)
  if (!("e_results_needed" %in% names(cmd_df))) {
    cmd_df$e_results_needed = I(replicate(nrow(cmd_df), character(0), simplify = FALSE))
  }
  if (!("r_results_needed" %in% names(cmd_df))) {
     cmd_df$r_results_needed = I(replicate(nrow(cmd_df), character(0), simplify = FALSE))
  }


  # --- First pass: Mark commands that are inherently data-modifying ---
  cmd_df$do_translate = cmd_df$stata_cmd %in% stata_data_manip_cmds

  # --- Second pass: Mark estimation/summary commands if their results are used ---
  for (i in seq_len(NROW(cmd_df))) {
    current_cmd = cmd_df$stata_cmd[i]
    current_line_num = cmd_df$line[i]

    # Check for r() usage from summarize/tabulate
    if (current_cmd %in% c("summarize", "su", "tabulate", "tab")) {
      needed_r_results = character(0)
      # Scan subsequent commands for r(...) usage
      for (j in (i + 1):NROW(cmd_df)) {
        # Check in rest_of_cmd (e.g., in `if` conditions or expressions)
        # This regex looks for r(anything_not_closing_paren)
        if (dplyr::coalesce(stringi::stri_detect_regex(cmd_df$rest_of_cmd[j], "r\\([^)]+\\)"), FALSE)) {
          # Extract specific r() values used, e.g., r(mean), r(N)
          matches = stringi::stri_match_all_regex(cmd_df$rest_of_cmd[j], "r\\(([^)]+)\\)")[[1]]
          if (NROW(matches) > 0) {
            needed_r_results = unique(c(needed_r_results, paste0("r(", matches[,2], ")")))
          }
        }
      }
      if (length(needed_r_results) > 0) {
        cmd_df$do_translate[i] = TRUE
        cmd_df$r_results_needed[[i]] = union(cmd_df$r_results_needed[[i]], needed_r_results)
      } else {
        # If no r() values are used, and it's summarize/tabulate (now in non_data_manip_cmds),
        # ensure it's not translated unless already marked true by another rule.
        # This part is implicitly handled by the initial `do_translate = cmd_df$stata_cmd %in% stata_data_manip_cmds`
        # and the later `stata_non_data_manip_cmds` override.
        # But explicitly ensuring it's FALSE if no r() is needed.
        cmd_df$do_translate[i] = FALSE
      }
    }

    # Check for e() usage from estimation commands
    if (current_cmd %in% stata_estimation_cmds) {
      needed_e_results = character(0)
      # Scan subsequent commands for e(...) usage
      for (j in (i + 1):NROW(cmd_df)) {
        if (dplyr::coalesce(stringi::stri_detect_regex(cmd_df$rest_of_cmd[j], "e\\([^)]+\\)"), FALSE)) {
          matches = stringi::stri_match_all_regex(cmd_df$rest_of_cmd[j], "e\\(([^)]+)\\)")[[1]]
          if (NROW(matches) > 0) {
            needed_e_results = unique(c(needed_e_results, paste0("e(", matches[,2], ")")))
          }
        }
      }
      if (length(needed_e_results) > 0) {
        cmd_df$do_translate[i] = TRUE
        cmd_df$e_results_needed[[i]] = union(cmd_df$e_results_needed[[i]], needed_e_results)
      }
    }
  }


  # --- Final explicit overrides ---
  # Commands that are definitely not data manipulation (e.g. `list`, `display` for scalars)
  # `format` only affects display, not data values.
  # These commands are now in stata_non_data_manip_cmds.
  # If a command is in stata_non_data_manip_cmds, it should be FALSE unless its r()/e() results are *explicitly* needed.
  for (k in seq_len(NROW(cmd_df))) {
      if (cmd_df$stata_cmd[k] %in% stata_non_data_manip_cmds) {
          # Only set to FALSE if it was not marked TRUE because its r() or e() results are needed.
          if (! (isTRUE(cmd_df$do_translate[k]) &&
                 (length(unlist(cmd_df$r_results_needed[k])) > 0 || length(unlist(cmd_df$e_results_needed[k])) > 0) ) ) {
              cmd_df$do_translate[k] = FALSE
          }
      }
  }


  # If stata_cmd is NA (e.g. empty line or parse error), don't translate
  cmd_df$do_translate[is.na(cmd_df$stata_cmd)] = FALSE

  # `clear` as a command clears memory. `use "file", clear` is different.
  # `clear` option is handled by `t_use`.
  if ("clear" %in% cmd_df$stata_cmd) {
      cmd_df$do_translate[cmd_df$stata_cmd == "clear" & (is.na(cmd_df$rest_of_cmd) | cmd_df$rest_of_cmd == "")] = TRUE # standalone clear
  }
  # Ensure 'save' is always translated.
  cmd_df$do_translate[cmd_df$stata_cmd == "save"] = TRUE


  # Ensure estimation commands that produce needed e() results are NOT turned off by is_quietly_prefix
  # This should be covered by the e_results_needed logic setting do_translate = TRUE.
  # The `is_quietly_prefix` flag from parsing can be used by t_ an t_ function if needed,
  # but should not gatekeep translation if results are used.

  return(cmd_df)
}

