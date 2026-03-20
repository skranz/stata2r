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
