mark_data_manip_cmd = function(cmd_df) {
  restore.point("mark_data_manip_cmd")

  if (NROW(cmd_df) == 0) {
    cmd_df$do_translate = logical(0)
    # Ensure e_results_needed and r_results_needed exist if cmd_df is empty but structured
    if (!("e_results_needed" %in% names(cmd_df))) cmd_df$e_results_needed = I(vector("list", 0))
    if (!("r_results_needed" %in% names(cmd_df))) cmd_df$r_results_needed = I(vector("list", 0))
    # NEW: Initialize new column for original_order_idx presence
    if (!("will_have_original_order_idx" %in% names(cmd_df))) cmd_df$will_have_original_order_idx = logical(0)
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
  # NEW: Initialize will_have_original_order_idx
  cmd_df$will_have_original_order_idx = rep(FALSE, NROW(cmd_df))


  # --- First pass: Mark commands that are inherently data-modifying ---
  # These commands *always* modify the dataset directly.
  cmd_df$do_translate = cmd_df$stata_cmd %in% stata_data_manip_cmds
  # Ensure 'save' is always translated. (It already is in stata_data_manip_cmds)


  # --- Second pass: Determine which commands produce e() or r() results that are *actually used* ---
  # Iterate backwards to find the *last* command producing a needed result.
  active_needed_e_results = character(0) # e.g., "e(sample)", "e(b)"
  active_needed_r_results = character(0) # e.g., "r(N)", "r(mean)"

  for (i in NROW(cmd_df):1) {
    current_cmd = cmd_df$stata_cmd[i]
    rest_of_cmd = dplyr::coalesce(cmd_df$rest_of_cmd[i], "")

    # Identify e() and r() usage in the current command's `rest_of_cmd`
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

    # If any of these used macros are currently needed from a *previous* producer, mark this command for translation
    if (any(used_e_macros %in% active_needed_e_results) || any(used_r_macros %in% active_needed_r_results)) {
        cmd_df$do_translate[i] = TRUE
    }

    # Add results that this command *would produce* to active_needed_e/r_results if they are not already.
    # And mark this command to translate if it produces a needed result.

    # Commands producing e() results (e.g., regress, xi)
    if (current_cmd %in% stata_estimation_cmds) {
      # List all e() results that this type of command can produce.
      # This list should be comprehensive for the command's potential outputs.
      potential_e_results_produced = c("e(sample)", "e(N)", "e(r2)", "e(df_r)", "e(rmse)", "e(b)", "e(V)") # Example for regress/xi
      
      # If any of these potential results are currently needed, then this command is the producer.
      if (any(potential_e_results_produced %in% active_needed_e_results)) {
          cmd_df$do_translate[i] = TRUE
          # Store which results this command should actually produce
          cmd_df$e_results_needed[[i]] = union(cmd_df$e_results_needed[[i]], intersect(potential_e_results_produced, active_needed_e_results))
          # Remove these from active_needed_e_results as we've found their producer
          active_needed_e_results = setdiff(active_needed_e_results, potential_e_results_produced)
      }
    }

    # Commands producing r() results (e.g., summarize, tabulate, count)
    if (current_cmd %in% stata_r_result_cmds) {
      # For now, only common r() values are tracked. Extend as needed.
      potential_r_results_produced = c("r(N)", "r(mean)", "r(sd)", "r(min)", "r(max)", "r(sum)", "r(p50)") # Example for summarize
      
      if (any(potential_r_results_produced %in% active_needed_r_results)) {
          cmd_df$do_translate[i] = TRUE
          cmd_df$r_results_needed[[i]] = union(cmd_df$r_results_needed[[i]], intersect(potential_r_results_produced, active_needed_r_results))
          active_needed_r_results = setdiff(active_needed_r_results, potential_r_results_produced)
      }
    }
    
    # Add any results used by *this* command to `active_needed_e/r_results` for prior commands to produce.
    # This must be done *after* checking if current command is a producer of these results.
    active_needed_e_results = union(active_needed_e_results, used_e_macros)
    active_needed_r_results = union(active_needed_r_results, used_r_macros)
  }

  # --- Third pass: Determine if stata2r_original_order_idx will be present at this line's execution ---
  # This is a forward pass.
  current_has_order_idx_at_translation_time = FALSE
  for (i in seq_len(NROW(cmd_df))) {
    if (cmd_df$do_translate[i]) { # Only consider translated commands
      if (cmd_df$stata_cmd[i] %in% c("use", "collapse", "reshape")) {
        current_has_order_idx_at_translation_time = TRUE # These commands create / re-create the index
      } else if (cmd_df$stata_cmd[i] %in% c("drop", "keep", "expand", "merge", "append", "order")) {
        # These commands modify rows but *maintain* the index if it exists.
        # So, the status remains as determined by prior commands.
        # No change to current_has_order_idx_at_translation_time here.
      } else if (cmd_df$stata_cmd[i] %in% c("preserve")) {
        # 'preserve' saves the current state. The index status doesn't change for the current data.
        # No change to current_has_order_idx_at_translation_time here.
      } else if (cmd_df$stata_cmd[i] %in% c("restore")) {
        # 'restore' restores a previous state. The `t_preserve_restore` function recreates
        # `stata2r_original_order_idx` if it existed in the preserved data.
        # So, for the purpose of translation-time `t_sort` logic, it's safer to consider `restore`
        # as preserving the `has_original_order_idx` state.
        # No explicit change to `current_has_order_idx_at_translation_time` here.
      }
    }
    cmd_df$will_have_original_order_idx[i] = current_has_order_idx_at_translation_time
  }


  # --- Final explicit overrides ---
  # Commands that are definitely not data manipulation (e.g. `list`, `display` for scalars)
  # If a command is in stata_non_data_manip_cmds, it should be FALSE unless its r() or e() results are *explicitly* needed.
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
  # Standalone `clear` command should be translated.
  if ("clear" %in% cmd_df$stata_cmd) {
      cmd_df$do_translate[cmd_df$stata_cmd == "clear" & (is.na(cmd_df$rest_of_cmd) | cmd_df$rest_of_cmd == "")] = TRUE # standalone clear
  }
  
  return(cmd_df)
}


