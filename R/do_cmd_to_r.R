  # r_obj will be a single row tibble
  # at least with the field r_code
do_cmd_to_r = function(cmd_obj, line, cmd_df) { # Corrected signature: added cmd_obj
  # cmd_obj is already the current line's data from cmd_df
  # line is the index, cmd_df is the full parsed do-file dataframe

  # ignore do commands that are flagged not to
  # be translated (because they don't manipulate the data set)
  if (!cmd_obj$do_translate || is.na(cmd_obj$stata_cmd)) {
    # Still return a structure that bind_rows expects, but with NA r_code
     return(data.frame(line=line, r_code = NA_character_, do_code = cmd_obj$do_code, stringsAsFactors = FALSE))
  }

  r_code = NA_character_

  # Context for expression translation (e.g. _n, _N behavior)
  translation_context = list(
    is_by_group = cmd_obj$is_by_prefix
    # can add more context like current data name if it changes from 'data'
  )

  # Dispatch to specific translation functions
  # These functions should take (cmd_obj, cmd_df, line_num, translation_context)
  # and return a string of R code.

  # Remove NA from rest_of_cmd for cleaner passing
  rest_of_cmd_clean = ifelse(is.na(cmd_obj$rest_of_cmd), "", cmd_obj$rest_of_cmd)

  # Use a switch for command dispatch
  # Each t_ function is responsible for parsing `rest_of_cmd_clean`
  # and using `translation_context`, `cmd_obj`, `cmd_df` as needed.
  # They should return a string of R code.
  stata_command = cmd_obj$stata_cmd

  # Store generated R variable names for tempfiles or r() values if needed to pass state
  # This is tricky with main.R's lapply. For now, variable names are deterministically generated.

  r_code = switch(stata_command,
    "use" = t_use(rest_of_cmd_clean, cmd_obj, cmd_df, line),
    "generate" = t_generate(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
    "gen" = t_generate(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context), # Alias
    "replace" = t_replace(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
    "summarize" = t_summarize(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
    "su" = t_summarize(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context), # Alias
    "egen" = t_egen(rest_of_cmd_clean, cmd_obj, cmd_df, line, translation_context),
    "sort" = t_sort(rest_of_cmd_clean, cmd_obj, cmd_df, line, type="sort"),
    "gsort" = t_sort(rest_of_cmd_clean, cmd_obj, cmd_df, line, type="gsort"),
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
    "encode" = t_encode(rest_of_cmd_clean, cmd_obj, cmd_df, line),
    "decode" = t_decode(rest_of_cmd_clean, cmd_obj, cmd_df, line),
    "destring" = t_destring(rest_of_cmd_clean, cmd_obj, cmd_df, line),
    "preserve" = t_preserve_restore(cmd_obj, type = "preserve"),
    "restore" = t_preserve_restore(cmd_obj, type = "restore"),
    # Add more commands here...
    # Fallback for unhandled but translatable commands:
    paste0("# Stata command '", cmd_obj$stata_cmd_original, " ", rest_of_cmd_clean, "' not yet fully translated.")
  )

  # If r_code is NULL (function not implemented), use default message
  if (is.null(r_code)) {
      r_code = paste0("# Stata command '", cmd_obj$stata_cmd_original, " ", rest_of_cmd_clean, "' (",stata_command,") translation not implemented.")
  }


  # Result object
  r_obj = data.frame(line=line, r_code = r_code, do_code = cmd_obj$do_code, stringsAsFactors = FALSE)
  return(r_obj)
}


