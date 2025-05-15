mark_data_manip_cmd = function(cmd_df) {
  # Determine commands that actually can transform the Stata data
  # set or generate scalars/temp files that will be used in later
  # commands that change the data set.

  if (NROW(cmd_df) == 0) {
    cmd_df$do_translate = logical(0)
    return(cmd_df)
  }

  cmd_df$do_translate = rep(FALSE, NROW(cmd_df))

  # Simple case: command is in stata_data_manip_cmds list
  cmd_df$do_translate = cmd_df$stata_cmd %in% stata_data_manip_cmds

  # Special handling for commands like `summarize`
  # If `summarize` produces `r()` results, and a later command uses them,
  # then `summarize` should be translated.
  # This is complex. For now, assume `summarize` that is not just for display (e.g. has options like meanonly, or is followed by r() usage)
  # For a first pass, mark all `summarize` as TRUE if it's in stata_data_manip_cmds.
  # A more sophisticated approach would involve checking for subsequent `r()` usage.

  # For commands like `tempfile`, they don't change `data` but set up for later changes.
  # These are already in `stata_data_manip_cmds`.

  # Commands that are definitely not data manipulation (e.g. `list`, `display` for scalars)
  # `display` can be used to show `r()` results. If those results are not part of data manip chain, ignore.
  # Non-data modifying commands or pure display commands.
  # These are marked FALSE even if they slip through the stata_data_manip_cmds list logic.
  non_manip_display_cmds = c("list", "display", "describe", "help", "about", "query", "set more off", "set rmsg on") # etc.
  cmd_df$do_translate[cmd_df$stata_cmd %in% non_manip_display_cmds] = FALSE

  # If stata_cmd is NA (e.g. empty line or parse error), don't translate
  cmd_df$do_translate[is.na(cmd_df$stata_cmd)] = FALSE

  # `clear` as a command clears memory, can be translated to `rm(list=ls())` or `data = NULL`
  # but `use "file", clear` is different. `clear` option is handled by `t_use`.
  # if `clear` is a standalone command and `stata_cmd` becomes "clear"
  if ("clear" %in% cmd_df$stata_cmd) {
      cmd_df$do_translate[cmd_df$stata_cmd == "clear" & is.na(cmd_df$rest_of_cmd)] = TRUE # standalone clear
  }


  # Example refinement for `summarize`:
  # Only translate `summarize` if it seems to be for `r()` values.
  # A simple proxy: if it has options (like `meanonly`, `detail`).
  # Or if a subsequent command refers to `r(...)`. (This lookahead is complex here).
  # For now, this is simplified. If `summarize` is in stata_data_manip_cmds, it's TRUE.

  return(cmd_df)
}

