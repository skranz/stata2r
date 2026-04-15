An issue with the R translation failing for runid 206 (`object 'lgdppcus' not found`) is traced back to how `collapse` operations were being translated when using Stata's variable abbreviation rules.

In Stata, the command `collapse ... lgdp ...` leverages abbreviation: since `lgdp` does not exist but `lgdppcus` does, Stata automatically expands it to `lgdppcus`. The output column name resulting from `collapse` then inherits this expanded name (`lgdppcus`), not the typed abbreviation (`lgdp`). 

The previous version of `t_collapse` statically hardcoded the source and target strings (e.g. `` `lgdp` ``) into the generated R script without expanding them via `names(data)`. While the evaluation phase inside `scmd_collapse` did resolve the underlying value to `lgdppcus`, it kept the target assignment key strictly bound to `` `lgdp` ``, causing subsequent commands looking for `lgdppcus` to fail.

We can fix this by completely refactoring `t_collapse` to pass the raw Stata parameters directly to the runtime function `scmd_collapse`. There, we can resolve all wildcards and abbreviations dynamically against `names(data)` *before* assigning target columns.

!MODIFICATION t_collapse R/t_collapse.R
scope = "file"
file = "R/t_collapse.R"
description = "Rewrite t_collapse to expand variable lists dynamically at runtime, ensuring correct target column names for abbreviations and wildcards."
---
```r
# FILE: R/t_collapse.R

s2r_parse_collapse_aggs = function(agg_part) {
  restore.point("s2r_parse_collapse_aggs")

  agg_part = stringi::stri_trim_both(agg_part)
  if (is.na(agg_part) || agg_part == "") {
    return(data.frame(
      stat = character(0),
      target_var = character(0),
      source_expr = character(0),
      stringsAsFactors = FALSE
    ))
  }

  agg_norm = stringi::stri_replace_all_regex(agg_part, "\\s*=\\s*", "=")

  stat_locs = stringi::stri_locate_all_regex(
    agg_norm,
    "\\([A-Za-z_][A-Za-z0-9_]*\\)"
  )[[1]]

  rows = vector("list", 0)

  add_segment = function(stat_name, segment_text) {
    segment_text = stringi::stri_trim_both(segment_text)
    if (is.na(segment_text) || segment_text == "") return(invisible(NULL))

    tokens = stringi::stri_split_regex(segment_text, "\\s+")[[1]]
    tokens = tokens[!is.na(tokens) & tokens != ""]
    if (length(tokens) == 0) return(invisible(NULL))

    for (tok in tokens) {
      if (stringi::stri_detect_fixed(tok, "=")) {
        parts = stringi::stri_split_fixed(tok, "=", n = 2)[[1]]
        target_var = stringi::stri_trim_both(parts[1])
        source_expr = stringi::stri_trim_both(parts[2])
      } else {
        target_var = stringi::stri_trim_both(tok)
        source_expr = NA_character_
      }

      if (is.na(target_var) || target_var == "") next

      rows[[length(rows) + 1]] <<- data.frame(
        stat = stat_name,
        target_var = target_var,
        source_expr = source_expr,
        stringsAsFactors = FALSE
      )
    }

    invisible(NULL)
  }

  if (NROW(stat_locs) == 0 || is.na(stat_locs[1, 1])) {
    add_segment("mean", agg_norm)
  } else {
    if (stat_locs[1, 1] > 1) {
      prefix_text = stringi::stri_trim_both(stringi::stri_sub(agg_norm, 1, stat_locs[1, 1] - 1))
      if (prefix_text != "") {
        add_segment("mean", prefix_text)
      }
    }

    for (i in seq_len(NROW(stat_locs))) {
      stat_text = stringi::stri_sub(
        agg_norm,
        stat_locs[i, 1] + 1,
        stat_locs[i, 2] - 1
      )

      seg_start = stat_locs[i, 2] + 1
      seg_end = if (i < NROW(stat_locs)) stat_locs[i + 1, 1] - 1 else stringi::stri_length(agg_norm)

      if (seg_start <= seg_end) {
        segment_text = stringi::stri_sub(agg_norm, seg_start, seg_end)
      } else {
        segment_text = ""
      }

      add_segment(stat_text, segment_text)
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      stat = character(0),
      target_var = character(0),
      source_expr = character(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, rows)
}

s2r_p_collapse = function(rest_of_cmd) {
  restore.point("s2r_p_collapse")
  parts = stringi::stri_match_first_regex(
    stringi::stri_trim_both(rest_of_cmd),
    "^\\s*(.*?)(?:,\\s*(.*))?$"
  )
  agg_part = stringi::stri_trim_both(parts[1, 2])
  options_part = stringi::stri_trim_both(parts[1, 3])

  parsed = s2r_parse_if_in(agg_part)
  agg_part = parsed$base_str

  by_vars = character(0)
  if (!is.na(options_part)) {
    by_opt = stringi::stri_match_first_regex(options_part, "\\bby\\s*\\(([^)]+)\\)")
    if (!is.na(by_opt[1, 1])) {
      by_vars = stringi::stri_split_regex(
        stringi::stri_trim_both(by_opt[1, 2]),
        "\\s+"
      )[[1]]
    }
  }

  list(
    aggs = s2r_parse_collapse_aggs(agg_part),
    by_vars = by_vars[by_vars != ""],
    if_str = parsed$if_str,
    in_str = parsed$in_str,
    raw_options = options_part
  )
}

t_collapse = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_collapse")
  parsed = s2r_p_collapse(rest_of_cmd)
  if (NROW(parsed$aggs) == 0) {
    return(paste0("# Failed to parse collapse aggregate definitions: ", rest_of_cmd))
  }

  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) {
    r_if_cond = translate_stata_expression_with_r_values(
      parsed$if_str,
      line_num,
      cmd_df,
      list(is_by_group = FALSE)
    )
  }

  source_expr_vals = ifelse(is.na(parsed$aggs$source_expr), "NA_character_", paste0("'", parsed$aggs$source_expr, "'"))

  aggs_df_str = paste0("data.frame(stat = c('", paste(parsed$aggs$stat, collapse="','"), "'), ",
                       "target_var = c('", paste(parsed$aggs$target_var, collapse="','"), "'), ",
                       "source_expr = c(", paste(source_expr_vals, collapse=", "), "), ",
                       "stringsAsFactors = FALSE)")

  args = c(
    "data = data",
    paste0("aggs_df = ", aggs_df_str)
  )
  if (length(parsed$by_vars) > 0) {
    args = c(
      args,
      paste0("group_vars = c('", paste(parsed$by_vars, collapse = "','"), "')")
    )
  }
  if (!is.na(r_if_cond)) {
    args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  }

  r_code = paste0("data = scmd_collapse(", paste(args, collapse = ", "), ")")
  r_code = paste0(r_code, "\nassign(\"has_original_order_idx\", TRUE, envir = stata2r_env)")

  return(r_code)
}

scmd_collapse = function(data, aggs_df, group_vars = character(0), r_if_cond = NA_character_) {
  restore.point("scmd_collapse")

  r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))

  if (!is.na(r_if_cond) && r_if_cond != "") {
    data = data[s2r_eval_cond(data, r_if_cond, envir = parent.frame()), , drop = FALSE]
  }

  group_vars_actual = expand_varlist(paste(group_vars, collapse = " "), names(data))

  agg_exprs = character(0)
  agg_names = character(0)

  for (i in seq_len(nrow(aggs_df))) {
    stat = aggs_df$stat[i]
    tgt = aggs_df$target_var[i]
    src = aggs_df$source_expr[i]

    if (is.na(src) || src == "") {
      src_vars = expand_varlist(tgt, names(data))
      tgt_vars = src_vars
    } else {
      src_vars = expand_varlist(src, names(data))
      if (length(src_vars) == 0) {
        src_vars = src
      }
      if (length(src_vars) > 1) {
        tgt_vars = paste0(tgt, seq_along(src_vars))
      } else {
        tgt_vars = tgt
      }
    }

    for (k in seq_along(src_vars)) {
      s_var = src_vars[k]
      t_var = tgt_vars[k]

      r_source = paste0("`", s_var, "`")

      collapse_func = switch(
        stat,
        "mean" = paste0("collapse::fmean(", r_source, ", na.rm = TRUE)"),
        "sum" = paste0("collapse::fsum(", r_source, ", na.rm = TRUE)"),
        "count" = paste0("sum(!is.na(", r_source, "))"),
        "N" = "dplyr::n()",
        "first" = paste0("collapse::ffirst(", r_source, ")"),
        "last" = paste0("collapse::flast(", r_source, ")"),
        "min" = paste0("collapse::fmin(", r_source, ", na.rm = TRUE)"),
        "max" = paste0("collapse::fmax(", r_source, ", na.rm = TRUE)"),
        "median" = paste0("collapse::fmedian(", r_source, ", na.rm = TRUE)"),
        "p50" = paste0("collapse::fmedian(", r_source, ", na.rm = TRUE)"),
        "sd" = paste0("collapse::fsd(", r_source, ", na.rm = TRUE)"),
        "p1" = paste0("collapse::fquantile(", r_source, ", probs = 0.01, na.rm = TRUE)"),
        "p5" = paste0("collapse::fquantile(", r_source, ", probs = 0.05, na.rm = TRUE)"),
        "p10" = paste0("collapse::fquantile(", r_source, ", probs = 0.10, na.rm = TRUE)"),
        "p25" = paste0("collapse::fquantile(", r_source, ", probs = 0.25, na.rm = TRUE)"),
        "p75" = paste0("collapse::fquantile(", r_source, ", probs = 0.75, na.rm = TRUE)"),
        "p90" = paste0("collapse::fquantile(", r_source, ", probs = 0.90, na.rm = TRUE)"),
        "p95" = paste0("collapse::fquantile(", r_source, ", probs = 0.95, na.rm = TRUE)"),
        "p99" = paste0("collapse::fquantile(", r_source, ", probs = 0.99, na.rm = TRUE)"),
        paste0("collapse::fmean(", r_source, ", na.rm = TRUE)")
      )

      agg_exprs = c(agg_exprs, collapse_func)
      agg_names = c(agg_names, t_var)
    }
  }

  pipe_el = c("data")
  if (length(group_vars_actual) > 0) {
    pipe_el = c(
      pipe_el,
      paste0("collapse::fgroup_by(", paste(paste0("`", group_vars_actual, "`"), collapse = ", "), ")")
    )
  }

  if (length(agg_exprs) > 0) {
    agg_str = paste(
      sprintf("`%s` = %s", agg_names, agg_exprs),
      collapse = ", "
    )
    pipe_el = c(pipe_el, paste0("collapse::fsummarise(", agg_str, ")"))
  } else {
    pipe_el = c(pipe_el, "dplyr::distinct()")
  }

  if (length(group_vars_actual) > 0 && length(agg_exprs) > 0) {
    pipe_el = c(pipe_el, "collapse::fungroup()")
  } else if (length(group_vars_actual) > 0) {
    pipe_el = c(pipe_el, "dplyr::ungroup()")
  }

  data = eval(
    parse(text = paste(pipe_el, collapse = " %>% ")),
    envir = list(data = data),
    enclos = parent.frame()
  )
  data$stata2r_original_order_idx = seq_len(nrow(data))
  return(data)
}
```
!END_MODIFICATION t_collapse R/t_collapse.R
