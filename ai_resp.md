The failure is caused by row order, not by a wrong computed variable.

In `do5`, the first mismatch appears right after

`bysort group_cat: egen mean_treated = mean(treated_obs)`

At that point, the Stata and R datasets have the same row count and the same columns, and `mean_treated` itself is not the problem. The problem is that `bysort` changes row order, and for ties in the sort key (`group_cat` alone here), Stata's tie order is not guaranteed to match the stable order produced by `dplyr::arrange()`. The current test machinery only ignores row order on the exact `sort`/`gsort` line, but not on later lines after a `bysort`-induced reorder. So the comparison becomes too strict and fails on a pure ordering difference.

The clean fix is to carry forward an "order may differ" flag after sort-like commands, and reset it only when a command rebuilds a fresh canonical dataset order such as `use`, `collapse`, or `reshape`.

!MODIFICATION mark_data_manip_cmd.R
scope = "file"
file = "R/mark_data_manip_cmd.R"
is_new_file = false
description = "Track row-order instability after sort-like commands so comparisons ignore pure ordering differences introduced by sort and bysort tie handling."
----------------------------------------------------------------------------------------------------------------------------------------------------------------

```r
s2r_cmd_resets_row_order_compare = function(cmd_obj) {
  stata_cmd = cmd_obj$stata_cmd[1]
  if (is.na(stata_cmd) || stata_cmd == "") return(FALSE)

  stata_cmd %in% c("use", "collapse", "reshape")
}

s2r_cmd_can_change_row_order = function(cmd_obj) {
  stata_cmd = cmd_obj$stata_cmd[1]
  if (is.na(stata_cmd) || stata_cmd == "") return(FALSE)

  if (stata_cmd %in% c("sort", "gsort")) {
    return(TRUE)
  }

  isTRUE(cmd_obj$is_bysort_prefix[1])
}

mark_data_manip_cmd = function(cmd_df) {
  restore.point("mark_data_manip_cmd")

  if (NROW(cmd_df) == 0) {
    cmd_df = s2r_check_mod_df(cmd_df)
    if (!("will_have_original_order_idx" %in% names(cmd_df))) cmd_df$will_have_original_order_idx = logical(0)
    if (!("will_ignore_row_order_for_comparison" %in% names(cmd_df))) cmd_df$will_ignore_row_order_for_comparison = logical(0)
    return(cmd_df)
  }

  cmd_df = s2r_check_mod_df(cmd_df)

  if (!("will_have_original_order_idx" %in% names(cmd_df))) {
    cmd_df$will_have_original_order_idx = rep(FALSE, NROW(cmd_df))
  }
  cmd_df$will_ignore_row_order_for_comparison = rep(FALSE, NROW(cmd_df))

  current_has_order_idx_at_translation_time = FALSE
  row_order_may_differ_from_stata = FALSE

  for (i in seq_len(NROW(cmd_df))) {
    cmd_obj = cmd_df[i, , drop = FALSE]

    if (s2r_cmd_resets_row_order_compare(cmd_obj)) {
      row_order_may_differ_from_stata = FALSE
    }

    if (cmd_df$do_translate[i]) {
      if (cmd_df$stata_cmd[i] %in% c("use", "collapse", "reshape")) {
        current_has_order_idx_at_translation_time = TRUE
      } else if (cmd_df$stata_cmd[i] %in% c("drop", "keep", "expand", "merge", "append", "order")) {
        current_has_order_idx_at_translation_time = current_has_order_idx_at_translation_time
      } else if (cmd_df$stata_cmd[i] %in% c("preserve")) {
        current_has_order_idx_at_translation_time = current_has_order_idx_at_translation_time
      } else if (cmd_df$stata_cmd[i] %in% c("restore")) {
        current_has_order_idx_at_translation_time = current_has_order_idx_at_translation_time
      }
    }
    cmd_df$will_have_original_order_idx[i] = current_has_order_idx_at_translation_time

    if (s2r_cmd_can_change_row_order(cmd_obj)) {
      row_order_may_differ_from_stata = TRUE
    }

    cmd_df$will_ignore_row_order_for_comparison[i] = row_order_may_differ_from_stata
  }

  return(cmd_df)
}
```

!END_MODIFICATION mark_data_manip_cmd.R

This fixes the failing test for the right reason: the row order after `bysort group_cat` is not semantically pinned down by the sort key, so the comparison should treat the rows as unordered until a later command recreates a fresh canonical order.
