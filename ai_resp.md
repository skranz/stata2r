!MODIFICATION t_tabulate.R
scope = "file"
file = "R/t_tabulate.R"
is_new_file = true
description = "Implement Stata tabulate with generate() side effects and minimal r() result storage."
-----------------------------------------------------------------------------------------------------

```r
# FILE: R/t_tabulate.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_tabulate = function(rest_of_cmd) {
  restore.point("s2r_p_tabulate")

  rest = stringi::stri_trim_both(rest_of_cmd)

  parts = stringi::stri_match_first_regex(
    rest,
    "^\\s*([^,]*?)(?:,\\s*(.*))?$"
  )

  main_part = stringi::stri_trim_both(parts[1, 2])
  options_str = stringi::stri_trim_both(parts[1, 3])

  parsed = s2r_parse_if_in(main_part)
  var_tokens = character(0)
  if (!is.na(parsed$base_str) && parsed$base_str != "") {
    var_tokens = stringi::stri_split_regex(parsed$base_str, "\\s+")[[1]]
    var_tokens = var_tokens[!is.na(var_tokens) & var_tokens != ""]
  }

  gen_stub = NA_character_
  if (!is.na(options_str) && options_str != "") {
    gen_match = stringi::stri_match_first_regex(
      options_str,
      "\\b(?:gen|generate)\\s*\\(([^)]+)\\)"
    )
    if (!is.na(gen_match[1, 1])) {
      gen_stub = stringi::stri_trim_both(gen_match[1, 2])
    }
  }

  include_missing = FALSE
  if (!is.na(options_str) && options_str != "") {
    include_missing = fast_coalesce(
      stringi::stri_detect_regex(options_str, "(^|[[:space:],])missing($|[[:space:],])"),
      FALSE
    )
  }

  list(
    var_tokens = var_tokens,
    if_str = parsed$if_str,
    in_str = parsed$in_str,
    options = options_str,
    gen_stub = gen_stub,
    include_missing = include_missing
  )
}

# 2. Code Generation Phase: Emit R code
t_tabulate = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_tabulate")

  parsed = s2r_p_tabulate(rest_of_cmd)
  needed_r = unlist(cmd_obj$r_results_needed)

  has_gen = !is.na(parsed$gen_stub) && parsed$gen_stub != ""

  if (!has_gen && length(needed_r) == 0) {
    return(paste0("# tabulate at line ", line_num, " is no-op (no generated dummies or later-used r())."))
  }

  if (length(parsed$var_tokens) == 0) {
    return(paste0("# Failed to parse tabulate command: ", rest_of_cmd))
  }

  if (has_gen && length(parsed$var_tokens) != 1) {
    return(paste0("# tabulate generate() is only implemented for one-way tabulate: ", rest_of_cmd))
  }

  varname = parsed$var_tokens[1]

  r_if_cond = NA_character_
  if (!is.na(parsed$if_str) && parsed$if_str != "") {
    r_if_cond = translate_stata_expression_with_r_values(
      parsed$if_str,
      line_num,
      cmd_df,
      list(is_by_group = FALSE)
    )
  }

  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  args = c(
    "data = data",
    paste0("varname = ", quote_for_r_literal(varname)),
    paste0("needed_r = ", s2r_chr_vec_to_r(needed_r))
  )

  if (has_gen) {
    args = c(args, paste0("gen_stub = ", quote_for_r_literal(parsed$gen_stub)))
  }
  if (!is.na(r_if_cond)) {
    args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  }
  if (!is.na(r_in_range)) {
    args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))
  }

  args = c(args, paste0("include_missing = ", parsed$include_missing))

  r_code = paste0("s2r_tab_res = scmd_tabulate(", paste(args, collapse = ", "), ")")

  if (has_gen) {
    r_code = paste0(r_code, "\ndata = s2r_tab_res$data")
  }

  if (length(needed_r) > 0) {
    r_code = paste0(r_code, "\ns2r_store_r_results(s2r_tab_res$r_results)")
  }

  return(r_code)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_tabulate = function(data, varname, needed_r = character(0), gen_stub = NA_character_,
                         r_if_cond = NA_character_, r_in_range = NA_character_,
                         include_missing = FALSE) {
  restore.point("scmd_tabulate")

  var_actual = expand_varlist(varname, names(data))[1]
  if (is.na(var_actual) || !(var_actual %in% names(data))) {
    stop(paste0("scmd_tabulate: variable '", varname, "' not found"))
  }

  n = NROW(data)
  mask = rep(TRUE, n)

  if (!is.na(r_if_cond) && r_if_cond != "") {
    r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))
    mask = mask & s2r_eval_cond(data, r_if_cond, envir = parent.frame())
  }

  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask = rep(FALSE, n)
    in_mask[idx] = TRUE
    mask = mask & in_mask
  }

  x = data[[var_actual]]
  x_for_tab = x[mask]

  is_missing_value = function(v) {
    if (is.character(v)) {
      return(is.na(v) | stringi::stri_trim_both(v) == "")
    }
    is.na(v)
  }

  missing_x_for_tab = is_missing_value(x_for_tab)

  if (!include_missing) {
    levels_source = x_for_tab[!missing_x_for_tab]
  } else {
    levels_source = x_for_tab
  }

  if (is.character(levels_source) || is.factor(levels_source)) {
    levels_chr = as.character(levels_source)
    levels_chr[is.na(levels_chr)] = ""
    levs = unique(levels_chr)
    levs = stringi::stri_sort(levs, locale = "C")
  } else {
    levs = sort(unique(as.numeric(levels_source)), na.last = TRUE)
  }

  r_results = list()
  if ("r(N)" %in% needed_r) {
    if (include_missing) {
      r_results$r_N = sum(mask)
    } else {
      r_results$r_N = sum(mask & !is_missing_value(x))
    }
  }

  has_gen = !is.na(gen_stub) && gen_stub != ""
  if (has_gen) {
    if (length(levs) > 0) {
      new_names = paste0(gen_stub, seq_along(levs))
      existing_new = intersect(new_names, names(data))
      if (length(existing_new) > 0) {
        stop(paste0(
          "scmd_tabulate: generated variable(s) already exist: ",
          paste(existing_new, collapse = ", ")
        ))
      }

      x_cmp = x
      if (is.character(x_cmp) || is.factor(x_cmp)) {
        x_cmp = as.character(x_cmp)
        x_cmp[is.na(x_cmp)] = ""
      } else {
        x_cmp = as.numeric(x_cmp)
      }

      for (i in seq_along(levs)) {
        level_i = levs[i]
        new_var = new_names[i]

        if (is.character(x_cmp)) {
          match_i = x_cmp == as.character(level_i)
        } else if (is.na(level_i)) {
          match_i = is.na(x_cmp)
        } else {
          match_i = x_cmp == level_i
        }

        match_i[is.na(match_i)] = FALSE

        out = rep(NA_integer_, n)
        out[mask] = 0L
        out[mask & match_i] = 1L

        data[[new_var]] = out
        attr(data[[new_var]], "label") = paste0(var_actual, "==", as.character(level_i))
      }
    }
  }

  return(list(data = data, r_results = r_results))
}
```

!END_MODIFICATION t_tabulate.R

!MODIFICATION do_cmd_to_r.R
scope = "file"
file = "R/do_cmd_to_r.R"
is_new_file = false
description = "Dispatch tabulate and tab commands to the new tabulate translator."
----------------------------------------------------------------------------------

```r
# r_obj will be a single row tibble
# at least with the field r_code
# r_obj will be a single row tibble
# at least with the field r_code
do_cmd_to_r = function(cmd_obj, line, cmd_df) {
  restore.point("do_cmd_to_r")

  if (!cmd_obj$do_translate || is.na(cmd_obj$stata_cmd)) {
    return(data.frame(
      line = line,
      r_code = NA_character_,
      do_code = cmd_obj$do_code,
      stata_translation_error = NA_character_,
      ignore_row_order_for_comparison = cmd_obj$will_ignore_row_order_for_comparison,
      stringsAsFactors = FALSE
    ))
  }

  r_code = NA_character_
  stata_translation_error = NA_character_

  translation_context = list(
    is_by_group = cmd_obj$is_by_prefix,
    is_quietly_prefix = cmd_obj$is_quietly_prefix,
    is_capture_prefix = cmd_obj$is_capture_prefix,
    is_xi_prefix = cmd_obj$is_xi_prefix,
    is_bysort_prefix = if ("is_bysort_prefix" %in% names(cmd_obj)) cmd_obj$is_bysort_prefix else FALSE
  )

  rest_of_cmd_clean = ifelse(is.na(cmd_obj$rest_of_cmd), "", cmd_obj$rest_of_cmd)
  stata_command = cmd_obj$stata_cmd

  res = tryCatch({
    r_code_translated = switch(stata_command,
      "use" = t_use(rest_of_cmd_clean, cmd_obj, cmd_df, line),
      "generate" = t_generate(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "gen" = t_generate(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "replace" = t_replace(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "summarize" = t_summarize(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "su" = t_summarize(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "tabulate" = t_tabulate(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "tab" = t_tabulate(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "egen" = t_egen(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "sort" = t_sort(rest_of_cmd_clean, cmd_obj, cmd_df, line, type = "sort"),
      "gsort" = t_sort(rest_of_cmd_clean, cmd_obj, cmd_df, line, type = "gsort"),
      "drop" = t_drop(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "keep" = t_keep(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "collapse" = t_collapse(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "rename" = t_rename(rest_of_cmd_clean, cmd_obj, cmd_df, line),
      "save" = t_save(rest_of_cmd_clean, cmd_obj, cmd_df, line),
      "tempfile" = t_tempfile(rest_of_cmd_clean, cmd_obj, cmd_df, line),
      "merge" = t_merge(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "append" = t_append(rest_of_cmd_clean, cmd_obj, cmd_df, line),
      "reshape" = t_reshape(rest_of_cmd_clean, cmd_obj, cmd_df, line),
      "recode" = t_recode(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "order" = t_order(rest_of_cmd_clean, cmd_obj, cmd_df, line),
      "expand" = t_expand(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "duplicates" = t_duplicates(rest_of_cmd_clean, cmd_obj, cmd_df, line),
      "encode" = t_encode(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "decode" = t_decode(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "destring" = t_destring(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "preserve" = t_preserve_restore(cmd_obj, type = "preserve"),
      "restore" = t_preserve_restore(cmd_obj, type = "restore"),
      "format" = t_format(rest_of_cmd_clean, cmd_obj, cmd_df, line),
      "label" = t_label(rest_of_cmd_clean, cmd_obj, cmd_df, line),
      "compress" = t_compress(rest_of_cmd_clean, cmd_obj, cmd_df, line),
      "regress" = t_regress(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "areg" = t_areg(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "xtreg" = t_xtreg(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "probit" = t_probit(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "reghdfe" = t_reghdfe(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "logit" = t_logit(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "ivregress" = t_ivregress(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "xi" = t_xi(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
      "scalar" = t_scalar(rest_of_cmd_clean, cmd_obj, cmd_df, line),
      "sc" = t_scalar(rest_of_cmd_clean, cmd_obj, cmd_df, line),
      paste0("# Stata command '", cmd_obj$stata_cmd_original, " ", rest_of_cmd_clean, "' not yet fully translated.")
    )

    if (is.null(r_code_translated)) {
      r_code_translated = paste0("# Stata command '", cmd_obj$stata_cmd_original, " ", rest_of_cmd_clean, "' (", stata_command, ") translation not implemented.")
    }

    if (isTRUE(translation_context$is_bysort_prefix)) {
      sort_vars = character(0)

      if (!is.na(cmd_obj$by_group_vars) && cmd_obj$by_group_vars != "") {
        sort_vars = c(sort_vars, stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]])
      }
      if (!is.na(cmd_obj$by_sort_vars) && cmd_obj$by_sort_vars != "") {
        sort_vars = c(sort_vars, stringi::stri_split_fixed(cmd_obj$by_sort_vars, ",")[[1]])
      }

      sort_vars = stringi::stri_trim_both(sort_vars)
      sort_vars = sort_vars[sort_vars != "" & !is.na(sort_vars)]

      if (length(sort_vars) > 0) {
        sort_code = paste0(
          "data = scmd_sort(data = data, varlist_str = ",
          quote_for_r_literal(paste(sort_vars, collapse = " ")),
          ", type = \"sort\")"
        )
        r_code_translated = paste(sort_code, r_code_translated, sep = "\n")
      }
    }

    list(r_code = r_code_translated, stata_translation_error = NA_character_)
  }, error = function(e) {
    list(
      r_code = paste0("# Translation failed for: ", cmd_obj$do_code, "\n# Error: ", e$message),
      stata_translation_error = e$message
    )
  })

  r_obj = data.frame(
    line = line,
    r_code = res$r_code,
    do_code = cmd_obj$do_code,
    stata_translation_error = res$stata_translation_error,
    ignore_row_order_for_comparison = cmd_obj$will_ignore_row_order_for_comparison,
    stringsAsFactors = FALSE
  )
  return(r_obj)
}
```

!END_MODIFICATION do_cmd_to_r.R

!MODIFICATION s2r_check_mod.R
scope = "file"
file = "R/s2r_check_mod.R"
is_new_file = false
description = "Mark tabulate commands with gen()/generate() as data-modifying while preserving r() dependency handling."
------------------------------------------------------------------------------------------------------------------------

```r
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
```

!END_MODIFICATION s2r_check_mod.R
