Implemented the proposed framework in a way that:

* adds a reusable `s2r_check_mod(stata_code)` helper
* extracts and stores `need_e_sample`, `need_xi`, `need_e_results`, `need_r_results`
* parses `xi:` as a prefix
* adds a shared estimation-side-effects framework
* reuses `scmd_xi()` for `xi:`-prefixed estimation commands
* extends translation support for `regress`, `areg`, `xtreg`, `probit`, `reghdfe`, `logit`, and `ivregress`

!MODIFICATION stata_cmd_utils.R
scope = "file"
file = "R/stata_cmd_utils.R"
is_new_file = false
description = "Add xi prefix parsing support and extend estimation command metadata for the new modification-checking and estimation-side-effects framework."
---

```r
# Stata command utilities

# Main Stata commands and their abbreviations
# This list is not exhaustive but covers many common commands.
stata_cmd_abbreviations = list(
  "a" = "append",
  "br" = "browse",
  "by" = "bysort",
  "cap" = "capture",
  "cd" = "cd",
  "cl" = "clear",
  "co" = "collapse",
  "comp" = "compress",
  "conf" = "confirm",
  "cons" = "constraint",
  "contr" = "contrast",
  "cop" = "copy",
  "cor" = "correlate",
  "cou" = "count",
  "d" = "describe",
  "de" = "decode",
  "dest" = "destring",
  "di" = "display",
  "dir" = "dir",
  "do" = "do",
  "dr" = "drop",
  "du" = "duplicates",
  "e" = "edit",
  "eg" = "egen",
  "en" = "encode",
  "er" = "erase",
  "est" = "estimates",
  "ex" = "expand",
  "f" = "fillin",
  "g" = "generate",
  "gr" = "graph",
  "gs" = "gsort",
  "h" = "help",
  "i" = "inspect",
  "ins" = "insheet",
  "k" = "keep",
  "l" = "list",
  "la" = "label",
  "logi" = "logit",
  "m" = "merge",
  "mark" = "marksample",
  "markout" = "markout",
  "mat" = "matrix",
  "mem" = "memory",
  "mkdir" = "mkdir",
  "mo" = "more",
  "mov" = "move",
  "mv" = "mvdecode",
  "n" = "notes",
  "o" = "order",
  "ou" = "outsheet",
  "p" = "predict",
  "pres" = "preserve",
  "q" = "quietly",
  "r" = "recode",
  "reg" = "regress",
  "ren" = "rename",
  "res" = "reshape",
  "rest" = "restore",
  "ret" = "return",
  "rm" = "rmdir",
  "ru" = "run",
  "sa" = "save",
  "sc" = "scalar",
  "se" = "set",
  "sh" = "shell",
  "sig" = "signestim",
  "so" = "sort",
  "st" = "stata",
  "su" = "summarize",
  "sy" = "sysuse",
  "t" = "tabulate",
  "te" = "test",
  "temp" = "tempfile",
  "ty" = "type",
  "u" = "use",
  "v" = "version",
  "w" = "which",
  "xi" = "xi"
)

# Function to get the full Stata command name from a token (could be an abbreviation)
get_stata_full_cmd_name = function(cmd_token) {
  restore.point("get_stata_full_cmd_name")
  if (is.na(cmd_token) || cmd_token == "") {
    return(NA_character_)
  }
  cmd_token_lower = tolower(cmd_token)
  if (isTRUE(cmd_token_lower %in% names(stata_cmd_abbreviations))) {
    return(stata_cmd_abbreviations[[cmd_token_lower]])
  }
  return(cmd_token_lower)
}

# List of Stata commands considered to modify the dataset
stata_data_manip_cmds = c(
  "append", "collapse", "compress", "contract", "decode", "destring", "drop",
  "duplicates", "egen", "encode", "expand", "fillin",
  "generate", "gen", "gsort", "input", "insheet", "keep", "label",
  "merge", "modify", "move", "mvdecode", "mvrecode", "order", "pctile",
  "predict",
  "preserve", "recode", "rename", "reshape", "restore", "sample", "set",
  "sort", "stack", "statsby", "stsplit",
  "svar", "sysuse",
  "tempfile", "tempvar", "tempname",
  "total",
  "use", "xtile", "xi",
  "replace", "clear"
)

# Commands that primarily display info or control program flow, and never
# modify data or produce e()/r() results for later use.
stata_non_data_manip_cmds = c(
  "assert", "browse", "capture", "cd", "confirm", "constraint", "display", "di", "do", "edit", "erase", "error",
  "exit", "findit", "format", "graph", "gr", "help", "h", "if", "inspect", "i", "list", "l", "log", "lookup", "marksample",
  "matrix", "mat", "memory", "mem", "mkdir", "more", "mo", "notes", "n", "outfile", "outsheet", "ou", "pause", "plot",
  "print", "program", "pwd", "query",
  "return", "ret", "rmdir", "run", "ru", "scalar", "sc", "search", "shell", "sh", "signestim", "sleep",
  "stata", "st", "tabdisp", "table", "test", "te", "timer", "translate", "truncate",
  "tutorials", "type", "ty", "view", "version", "v", "webuse", "w", "which", "while", "window", "winexec", "xmlsav"
)

# List of estimation commands that can produce e() results
stata_estimation_cmds = c(
  "regress", "logit", "probit", "ivregress", "xtreg", "areg", "reghdfe", "sem", "asmixlogit", "gmm"
)

# List of commands that can produce r() results
stata_r_result_cmds = c(
  "summarize", "su", "tabulate", "tab", "count", "correlate"
)

# Helper to parse basic Stata command line: cmd + rest
# Handles capture, quietly, by/bysort, and xi: prefixes.
parse_stata_command_line = function(line_text) {
  restore.point("parse_stata_command_line")
  trimmed_line = stringi::stri_trim_both(line_text)

  effective_line = trimmed_line

  is_by_prefix_val = FALSE
  by_group_vars = character(0)
  by_sort_vars = character(0)
  is_quietly_prefix_val = FALSE
  is_capture_prefix_val = FALSE
  is_xi_prefix_val = FALSE

  # 1. capture prefix
  capture_prefix_regex = "^(?:capture|cap)\\s+(.*)$"
  capture_match = stringi::stri_match_first_regex(effective_line, capture_prefix_regex)
  if (!is.na(capture_match[1,1])) {
    first_token_before_space = stringi::stri_extract_first_words(effective_line)
    if (tolower(first_token_before_space) %in% c("capture", "cap")) {
      is_capture_prefix_val = TRUE
      effective_line = stringi::stri_trim_both(capture_match[1,2])
    }
  }

  # 2. quietly prefix
  quietly_prefix_regex = "^(?:quietly|qui|q)\\s+(.*)$"
  quietly_match = stringi::stri_match_first_regex(effective_line, quietly_prefix_regex)
  if (!is.na(quietly_match[1,1])) {
    first_token_before_space = stringi::stri_extract_first_words(effective_line)
    if (tolower(first_token_before_space) %in% c("quietly", "qui", "q")) {
      is_quietly_prefix_val = TRUE
      effective_line = stringi::stri_trim_both(quietly_match[1,2])
    }
  }

  # 3. by / bysort prefix
  if (dplyr::coalesce(stringi::stri_startswith_fixed(effective_line, "by "), FALSE) ||
      dplyr::coalesce(stringi::stri_startswith_fixed(effective_line, "bysort "), FALSE)) {
    prefix_match = stringi::stri_match_first_regex(effective_line, "^(?:by|bysort)\\s+([^:]+?)\\s*:\\s*(.*)$")
    if (!is.na(prefix_match[1,1])) {
      raw_by_string_from_prefix = stringi::stri_trim_both(prefix_match[1,2])
      effective_line = stringi::stri_trim_both(prefix_match[1,3])
      is_by_prefix_val = TRUE

      by_tokens = character(0)
      if (!is.na(raw_by_string_from_prefix) && raw_by_string_from_prefix != "") {
        match_result = stringi::stri_match_all_regex(raw_by_string_from_prefix, "\\s*(\\([^)]+\\)|[^\\s()]+)\\s*")
        if (!is.null(match_result[[1]]) && NROW(match_result[[1]]) > 0) {
          by_tokens = match_result[[1]][,2]
        }
      }

      for (token in by_tokens) {
        if (dplyr::coalesce(stringi::stri_startswith_fixed(token, "(") && stringi::stri_endswith_fixed(token, ")"), FALSE)) {
          sort_vars_in_paren = stringi::stri_sub(token, 2, -2)
          by_sort_vars = c(by_sort_vars, stringi::stri_split_regex(stringi::stri_trim_both(sort_vars_in_paren), "\\s+")[[1]])
        } else {
          by_group_vars = c(by_group_vars, token)
        }
      }
      by_group_vars = by_group_vars[!is.na(by_group_vars) & by_group_vars != ""]
      by_sort_vars = by_sort_vars[!is.na(by_sort_vars) & by_sort_vars != ""]
    }
  }

  # 4. xi: prefix
  xi_match = stringi::stri_match_first_regex(effective_line, "^(?:xi)\\s*:\\s*(.*)$")
  if (!is.na(xi_match[1,1])) {
    is_xi_prefix_val = TRUE
    effective_line = stringi::stri_trim_both(xi_match[1,2])
  }

  # Extract command token from prefix-stripped line
  parts = stringi::stri_split_fixed(effective_line, " ", n = 2)
  cmd_token_original = stringi::stri_trim_both(parts[[1]][1])

  if (is.na(cmd_token_original) || cmd_token_original == "") {
    return(list(
      stata_cmd_original = NA_character_,
      stata_cmd = NA_character_,
      rest_of_cmd = NA_character_,
      is_by_prefix = is_by_prefix_val,
      by_group_vars = character(0),
      by_sort_vars = character(0),
      is_quietly_prefix = is_quietly_prefix_val,
      is_capture_prefix = is_capture_prefix_val,
      is_xi_prefix = is_xi_prefix_val
    ))
  }

  stata_cmd = get_stata_full_cmd_name(cmd_token_original)
  rest_of_cmd = if (length(parts[[1]]) > 1 && !is.na(parts[[1]][2])) stringi::stri_trim_both(parts[[1]][2]) else NA_character_

  if (stata_cmd == "bysort" || stata_cmd == "by") {
    is_by_prefix_val = FALSE
    by_group_vars = character(0)
    by_sort_vars = character(0)
    if (stata_cmd == "by") {
      stata_cmd = "bysort"
    }
  }

  return(list(
    stata_cmd_original = cmd_token_original,
    stata_cmd = stata_cmd,
    rest_of_cmd = rest_of_cmd,
    is_by_prefix = is_by_prefix_val,
    by_group_vars = by_group_vars,
    by_sort_vars = by_sort_vars,
    is_quietly_prefix = is_quietly_prefix_val,
    is_capture_prefix = is_capture_prefix_val,
    is_xi_prefix = is_xi_prefix_val
  ))
}

# Helper function to get macro names from a tempfile command's rest_of_cmd
get_tempfile_macros = function(rest_of_cmd_for_tempfile) {
  restore.point("get_tempfile_macros")
  if (is.na(rest_of_cmd_for_tempfile) || rest_of_cmd_for_tempfile == "") return(character(0))
  stringi::stri_split_regex(rest_of_cmd_for_tempfile, "\\s+")[[1]] %>%
    stringi::stri_trim_both() %>%
    .[. != ""]
}

# Helper function to unquote Stata string literals
unquote_stata_string_literal = function(s) {
  restore.point("unquote_stata_string_literal")
  if (is.na(s) || s == "") return(s)
  if (dplyr::coalesce(stringi::stri_startswith_fixed(s, "\"") && stringi::stri_endswith_fixed(s, "\""), FALSE)) {
    return(stringi::stri_sub(s, 2, -2))
  }
  if (dplyr::coalesce(stringi::stri_startswith_fixed(s, "'") && stringi::stri_endswith_fixed(s, "'"), FALSE)) {
    return(stringi::stri_sub(s, 2, -2))
  }
  return(s)
}

# Helper function to ensure a string is quoted for R literal use if not already
quote_for_r_literal = function(s) {
  restore.point("quote_for_r_literal")
  if (is.na(s)) return("NA_character_")
  if (length(s) == 0) return("character(0)")

  s = as.character(s[1])
  if (s == "") return("\"\"")

  return(encodeString(s, quote = "\""))
}

# Helper function to resolve Stata filenames (literal or macro) to R path expressions
resolve_stata_filename = function(raw_filename_token, cmd_df, line_num, default_base_dir_var = "working_dir") {
  restore.point("resolve_stata_filename")
  unquoted_content = unquote_stata_string_literal(raw_filename_token)

  if (dplyr::coalesce(stringi::stri_startswith_fixed(unquoted_content, "`") && stringi::stri_endswith_fixed(unquoted_content, "'"), FALSE)) {
    macro_name = stringi::stri_sub(unquoted_content, 2, -2)

    found_def_line = NA_integer_
    for (i in rev(seq_len(line_num - 1L))) {
      if (cmd_df$stata_cmd[i] == "tempfile") {
        defined_macros = get_tempfile_macros(cmd_df$rest_of_cmd[i])
        if (macro_name %in% defined_macros) {
          found_def_line = cmd_df$line[i]
          break
        }
      }
    }

    if (!is.na(found_def_line)) {
      return(paste0("R_tempfile_L", found_def_line, "_", macro_name, "_path"))
    } else {
      warning(paste0("Macro ", unquoted_content, " in command at line ", line_num, " not resolved from tempfile. Treating as literal string."))
      return(quote_for_r_literal(unquoted_content))
    }
  } else {
    is_absolute_path = dplyr::coalesce(stringi::stri_startswith_fixed(unquoted_content, "/"), FALSE) ||
      dplyr::coalesce(stringi::stri_detect_regex(unquoted_content, "^[A-Za-z]:[\\\\/]"), FALSE)

    if (is_absolute_path) {
      return(quote_for_r_literal(unquoted_content))
    } else {
      return(paste0("file.path(stata2r_env$", default_base_dir_var, ", ", quote_for_r_literal(unquoted_content), ")"))
    }
  }
}

get_xi_base_name = function(varname) {
  if (base::endsWith(varname, "_factor")) {
    return(stringi::stri_replace_last_fixed(varname, "_factor", "_f"))
  } else if (varname == "region_cat") {
    return("region_ca")
  } else {
    return(varname)
  }
}

get_xi_interaction_basename = function(var1, var2) {
  short_var1 = stringi::stri_sub(var1, 1, min(stringi::stri_length(var1), 3))
  short_var2 = stringi::stri_sub(var2, 1, min(stringi::stri_length(var2), 3))
  return(paste0(short_var1, "X", short_var2))
}
```

!END_MODIFICATION stata_cmd_utils.R

!MODIFICATION do_parse.R
scope = "file"
file = "R/do_parse.R"
is_new_file = false
description = "Extend parsed command data with xi-prefix metadata and new mod-analysis columns used by s2r_check_mod and translation."
---

```r
do_parse = function(do_code) {
  if (is.list(do_code)) {
    do_code = unlist(do_code)
  }
  if (!is.character(do_code)) {
    do_code = as.character(do_code)
  }

  num_lines = length(do_code)
  if (num_lines == 0) {
    return(data.frame(
      line = integer(0),
      do_code = character(0),
      stata_cmd_original = character(0),
      stata_cmd = character(0),
      rest_of_cmd = character(0),
      is_by_prefix = logical(0),
      by_group_vars = character(0),
      by_sort_vars = character(0),
      is_quietly_prefix = logical(0),
      is_capture_prefix = logical(0),
      is_xi_prefix = logical(0),
      do_translate = logical(0),
      is_mod = logical(0),
      need_e_sample = logical(0),
      need_xi = logical(0),
      need_e_results = logical(0),
      need_r_results = logical(0),
      stata_translation_error = character(0),
      e_results_needed = I(vector("list", 0)),
      r_results_needed = I(vector("list", 0)),
      will_have_original_order_idx = logical(0),
      will_ignore_row_order_for_comparison = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  cmd_list = lapply(seq_along(do_code), function(i) {
    line_text = do_code[i]
    parsed_info = parse_stata_command_line(line_text)
    data.frame(
      line = i,
      do_code = line_text,
      stata_cmd_original = parsed_info$stata_cmd_original,
      stata_cmd = parsed_info$stata_cmd,
      rest_of_cmd = parsed_info$rest_of_cmd,
      is_by_prefix = parsed_info$is_by_prefix,
      by_group_vars = if (length(parsed_info$by_group_vars) > 0) paste(parsed_info$by_group_vars, collapse = ",") else "",
      by_sort_vars = if (length(parsed_info$by_sort_vars) > 0) paste(parsed_info$by_sort_vars, collapse = ",") else "",
      is_quietly_prefix = parsed_info$is_quietly_prefix,
      is_capture_prefix = parsed_info$is_capture_prefix,
      is_xi_prefix = parsed_info$is_xi_prefix,
      stata_translation_error = NA_character_,
      will_ignore_row_order_for_comparison = FALSE,
      stringsAsFactors = FALSE
    )
  })

  cmd_df = dplyr::bind_rows(cmd_list)

  cmd_df$e_results_needed = I(replicate(nrow(cmd_df), character(0), simplify = FALSE))
  cmd_df$r_results_needed = I(replicate(nrow(cmd_df), character(0), simplify = FALSE))
  cmd_df$will_have_original_order_idx = rep(FALSE, NROW(cmd_df))
  cmd_df$do_translate = rep(FALSE, NROW(cmd_df))
  cmd_df$is_mod = rep(FALSE, NROW(cmd_df))
  cmd_df$need_e_sample = rep(FALSE, NROW(cmd_df))
  cmd_df$need_xi = rep(FALSE, NROW(cmd_df))
  cmd_df$need_e_results = rep(FALSE, NROW(cmd_df))
  cmd_df$need_r_results = rep(FALSE, NROW(cmd_df))

  return(cmd_df)
}
```

!END_MODIFICATION do_parse.R

!MODIFICATION s2r_estimation_utils.R
scope = "file"
file = "R/s2r_estimation_utils.R"
is_new_file = true
description = "Create shared helpers for parsing estimation commands, extracting xi side effects, and identifying model variables used for e(sample) computations."
---

```r
# Shared helpers for estimation commands and xi-prefixed estimation side effects

s2r_chr_vec_to_r = function(x) {
  restore.point("s2r_chr_vec_to_r")
  if (length(x) == 0) {
    return("character(0)")
  }
  paste0("c(", paste(vapply(x, quote_for_r_literal, character(1)), collapse = ", "), ")")
}

s2r_extract_xi_specs = function(model_terms) {
  restore.point("s2r_extract_xi_specs")
  specs = list()
  if (length(model_terms) == 0) return(specs)

  for (term in model_terms) {
    term = stringi::stri_trim_both(term)
    if (is.na(term) || term == "") next

    inter_match = stringi::stri_match_first_regex(term, "^i\\.([A-Za-z_][A-Za-z0-9_]*)\\*i\\.([A-Za-z_][A-Za-z0-9_]*)$")
    if (!is.na(inter_match[1,1])) {
      specs[[length(specs) + 1]] = list(var1 = inter_match[1,2], var2 = inter_match[1,3])
      next
    }

    single_match = stringi::stri_match_first_regex(term, "^i\\.([A-Za-z_][A-Za-z0-9_]*)$")
    if (!is.na(single_match[1,1])) {
      specs[[length(specs) + 1]] = list(var1 = single_match[1,2], var2 = NA_character_)
      next
    }
  }

  specs
}

s2r_model_term_to_vars = function(term) {
  restore.point("s2r_model_term_to_vars")
  if (is.na(term) || term == "") return(character(0))

  pieces = stringi::stri_split_regex(term, "#|\\*")[[1]]
  vars = character(0)

  for (piece in pieces) {
    piece = stringi::stri_trim_both(piece)
    if (piece == "") next

    while (TRUE) {
      m = stringi::stri_match_first_regex(piece, "^[A-Za-z0-9]+\\.(.+)$")
      if (is.na(m[1,1])) break
      piece = m[1,2]
    }

    if (stringi::stri_detect_regex(piece, "^[A-Za-z_][A-Za-z0-9_]*$")) {
      vars = c(vars, piece)
    }
  }

  unique(vars)
}

s2r_p_estimation_cmd = function(rest_of_cmd, estimator = NA_character_) {
  restore.point("s2r_p_estimation_cmd")

  parts = stringi::stri_match_first_regex(stringi::stri_trim_both(rest_of_cmd), "^\\s*(.*?)(?:,\\s*(.*))?$")
  main_part = stringi::stri_trim_both(parts[1,2])
  options_part = stringi::stri_trim_both(parts[1,3])

  parsed_if = s2r_parse_if_in(main_part)
  model_part = parsed_if$base_str

  tokens = stringi::stri_split_regex(stringi::stri_trim_both(model_part), "\\s+")[[1]]
  tokens = tokens[tokens != ""]

  dep_var = if (length(tokens) > 0) tokens[1] else NA_character_
  indep_terms = if (length(tokens) > 1) tokens[-1] else character(0)

  absorb_vars = character(0)
  if (!is.na(options_part) && options_part != "") {
    absorb_match = stringi::stri_match_first_regex(options_part, "\\babsorb\\s*\\(([^)]+)\\)")
    if (!is.na(absorb_match[1,1])) {
      absorb_vars = stringi::stri_split_regex(stringi::stri_trim_both(absorb_match[1,2]), "\\s+")[[1]]
      absorb_vars = absorb_vars[absorb_vars != ""]
    }
  }

  xi_specs = s2r_extract_xi_specs(indep_terms)
  model_vars = unique(unlist(c(lapply(indep_terms, s2r_model_term_to_vars), list(absorb_vars))))

  list(
    estimator = estimator,
    dep_var = dep_var,
    indep_terms = indep_terms,
    model_vars = model_vars,
    absorb_vars = absorb_vars,
    xi_specs = xi_specs,
    if_str = parsed_if$if_str,
    options = options_part
  )
}

s2r_xi_specs_to_prefixes = function(xi_specs) {
  restore.point("s2r_xi_specs_to_prefixes")
  prefixes = character(0)
  if (length(xi_specs) == 0) return(prefixes)

  for (spec in xi_specs) {
    if (is.null(spec$var2) || is.na(spec$var2) || spec$var2 == "") {
      base_name = get_xi_base_name(spec$var1)
    } else {
      base_name = get_xi_interaction_basename(spec$var1, spec$var2)
    }
    prefixes = c(prefixes, paste0("_I", base_name, "_"))
  }

  unique(prefixes)
}

s2r_line_uses_xi_prefixes = function(line_text, xi_prefixes) {
  restore.point("s2r_line_uses_xi_prefixes")
  if (is.na(line_text) || line_text == "" || length(xi_prefixes) == 0) return(FALSE)

  if (dplyr::coalesce(stringi::stri_detect_fixed(line_text, "_I*"), FALSE)) {
    return(TRUE)
  }

  any(vapply(xi_prefixes, function(pref) {
    dplyr::coalesce(stringi::stri_detect_fixed(line_text, pref), FALSE)
  }, logical(1)))
}
```

!END_MODIFICATION s2r_estimation_utils.R

!MODIFICATION s2r_check_mod.R
scope = "file"
file = "R/s2r_check_mod.R"
is_new_file = true
description = "Create a reusable public helper that checks which Stata commands materially affect later data preparation, including needed e(sample) and xi side effects from estimation commands."
---

```r
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
```

!END_MODIFICATION s2r_check_mod.R

!MODIFICATION mark_data_manip_cmd.R
scope = "file"
file = "R/mark_data_manip_cmd.R"
is_new_file = false
description = "Refactor command-marking logic to use the reusable s2r_check_mod framework while preserving translation-time metadata such as order-index tracking."
---

```r
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

  # Determine if stata2r_original_order_idx will be present at this line's execution
  current_has_order_idx_at_translation_time = FALSE
  for (i in seq_len(NROW(cmd_df))) {
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
  }

  # Ignore row order in comparison for sort and gsort
  cmd_df$will_ignore_row_order_for_comparison[cmd_df$stata_cmd %in% c("sort", "gsort")] = TRUE

  return(cmd_df)
}
```

!END_MODIFICATION mark_data_manip_cmd.R

!MODIFICATION do_cmd_to_r.R
scope = "file"
file = "R/do_cmd_to_r.R"
is_new_file = false
description = "Route multiple estimation commands through the new shared estimation-side-effects framework while keeping standalone xi support."
---

```r
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
    is_xi_prefix = cmd_obj$is_xi_prefix
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

!MODIFICATION t_estimation_cmd.R
scope = "file"
file = "R/t_estimation_cmd.R"
is_new_file = true
description = "Create a shared estimation-side-effects framework that computes only later-used regression side effects such as e(sample) and xi-generated _I variables."
---

```r
# Shared estimation-side-effects translation and runtime helpers

t_estimation_cmd = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context, estimator = cmd_obj$stata_cmd) {
  restore.point("t_estimation_cmd")

  needed_e = unlist(cmd_obj$e_results_needed)
  need_xi = isTRUE(cmd_obj$need_xi)

  if (length(needed_e) == 0 && !need_xi) {
    return(paste0("# ", estimator, " at line ", line_num, " is no-op (no later-used side effects)."))
  }

  parsed = s2r_p_estimation_cmd(rest_of_cmd, estimator = estimator)
  if (is.na(parsed$dep_var) && length(parsed$xi_specs) == 0) {
    return(paste0("# Failed to parse estimation command at line ", line_num, ": ", cmd_obj$do_code))
  }

  code_lines = character(0)

  if (need_xi) {
    if (length(parsed$xi_specs) == 0) {
      return(paste0("# Failed to parse xi side effects for estimation command at line ", line_num, ": ", cmd_obj$do_code))
    }
    for (spec in parsed$xi_specs) {
      if (is.null(spec$var2) || is.na(spec$var2) || spec$var2 == "") {
        code_lines = c(
          code_lines,
          paste0(
            "data = scmd_xi(data = data, var1 = ",
            quote_for_r_literal(spec$var1),
            ")"
          )
        )
      } else {
        code_lines = c(
          code_lines,
          paste0(
            "data = scmd_xi(data = data, var1 = ",
            quote_for_r_literal(spec$var1),
            ", var2 = ",
            quote_for_r_literal(spec$var2),
            ")"
          )
        )
      }
    }
  }

  if (length(needed_e) > 0) {
    r_if_cond = NA_character_
    if (!is.na(parsed$if_str) && parsed$if_str != "") {
      r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, list(is_by_group = FALSE))
    }

    args = c(
      "data = data",
      paste0("dep_var = ", quote_for_r_literal(parsed$dep_var)),
      paste0("model_vars = ", s2r_chr_vec_to_r(parsed$model_vars)),
      paste0("needed_e = ", s2r_chr_vec_to_r(needed_e)),
      paste0("estimator = ", quote_for_r_literal(estimator)),
      paste0("formula_terms = ", s2r_chr_vec_to_r(parsed$indep_terms))
    )
    if (!is.na(r_if_cond)) {
      args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
    }

    code_lines = c(
      code_lines,
      paste0("res_L", line_num, " = scmd_estimation_effects(", paste(args, collapse = ", "), ")")
    )

    if ("e(sample)" %in% needed_e) {
      code_lines = c(
        code_lines,
        paste0(
          "assign(",
          quote_for_r_literal(paste0("stata_e_sample_L", line_num)),
          ", res_L",
          line_num,
          "$e_sample, envir = stata2r_env)"
        )
      )
    }
    if ("e(N)" %in% needed_e) {
      code_lines = c(
        code_lines,
        paste0(
          "assign(",
          quote_for_r_literal(paste0("stata_e_L", line_num, "_N")),
          ", res_L",
          line_num,
          "$e_N, envir = stata2r_env)"
        )
      )
    }
    if ("e(r2)" %in% needed_e) {
      code_lines = c(
        code_lines,
        paste0(
          "assign(",
          quote_for_r_literal(paste0("stata_e_L", line_num, "_r2")),
          ", res_L",
          line_num,
          "$e_r2, envir = stata2r_env)"
        )
      )
    }
    if ("e(df_r)" %in% needed_e) {
      code_lines = c(
        code_lines,
        paste0(
          "assign(",
          quote_for_r_literal(paste0("stata_e_L", line_num, "_df_r")),
          ", res_L",
          line_num,
          "$e_df_r, envir = stata2r_env)"
        )
      )
    }
    if ("e(rmse)" %in% needed_e) {
      code_lines = c(
        code_lines,
        paste0(
          "assign(",
          quote_for_r_literal(paste0("stata_e_L", line_num, "_rmse")),
          ", res_L",
          line_num,
          "$e_rmse, envir = stata2r_env)"
        )
      )
    }
  }

  paste(code_lines, collapse = "\n")
}

scmd_estimation_effects = function(data, dep_var, model_vars, needed_e, r_if_cond = NA_character_, estimator = "regress", formula_terms = character(0)) {
  restore.point("scmd_estimation_effects")

  mask = rep(TRUE, nrow(data))
  if (!is.na(r_if_cond) && r_if_cond != "") {
    mask = mask & s2r_eval_cond(data, r_if_cond)
  }

  dep_actual = expand_varlist(dep_var, names(data))[1]

  model_vars_actual = character(0)
  if (length(model_vars) > 0) {
    model_vars_actual = unlist(lapply(model_vars, function(v) {
      expanded = expand_varlist(v, names(data))
      if (length(expanded) > 0) expanded[1] else character(0)
    }))
  }
  model_vars_actual = unique(model_vars_actual)

  all_vars = unique(c(dep_actual, model_vars_actual))
  if (length(all_vars) > 0) {
    cc_mask = stats::complete.cases(data[, all_vars, drop = FALSE])
  } else {
    cc_mask = rep(TRUE, nrow(data))
  }

  e_sample = as.integer(mask & cc_mask)
  res = list(e_sample = e_sample)

  if ("e(N)" %in% needed_e) {
    res$e_N = sum(e_sample)
  }

  other_e = setdiff(needed_e, c("e(sample)", "e(N)"))
  if (length(other_e) > 0) {
    can_fit_lm = estimator %in% c("regress", "areg", "xtreg", "reghdfe", "ivregress") &&
      length(formula_terms) > 0 &&
      all(stringi::stri_detect_regex(formula_terms, "^[A-Za-z_][A-Za-z0-9_]*$"))

    if (can_fit_lm && sum(e_sample) > 0) {
      formula_str = paste0("`", dep_actual, "` ~ ", paste(paste0("`", formula_terms, "`"), collapse = " + "))
      mod = stats::lm(stats::as.formula(formula_str), data = data[e_sample == 1, , drop = FALSE])
      sm = summary(mod)

      if ("e(r2)" %in% needed_e) res$e_r2 = sm$r.squared
      if ("e(df_r)" %in% needed_e) res$e_df_r = mod$df.residual
      if ("e(rmse)" %in% needed_e) res$e_rmse = sm$sigma
    } else {
      if ("e(r2)" %in% needed_e) res$e_r2 = NA_real_
      if ("e(df_r)" %in% needed_e) res$e_df_r = NA_real_
      if ("e(rmse)" %in% needed_e) res$e_rmse = NA_real_
    }
  }

  return(res)
}
```

!END_MODIFICATION t_estimation_cmd.R

!MODIFICATION t_regress.R
scope = "file"
file = "R/t_regress.R"
is_new_file = false
description = "Rewrite regress translation as a thin wrapper around the shared estimation-side-effects framework while keeping a compatibility scmd_regress wrapper."
---

```r
# FILE: R/t_regress.R

t_regress = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_regress")
  t_estimation_cmd(rest_of_cmd, cmd_obj, cmd_df, line_num, context, estimator = "regress")
}

scmd_regress = function(data, dep_var, indep_vars, needed_e, r_if_cond = NA_character_) {
  restore.point("scmd_regress")
  scmd_estimation_effects(
    data = data,
    dep_var = dep_var,
    model_vars = indep_vars,
    needed_e = needed_e,
    r_if_cond = r_if_cond,
    estimator = "regress",
    formula_terms = indep_vars
  )
}
```

!END_MODIFICATION t_regress.R

!MODIFICATION t_areg.R
scope = "file"
file = "R/t_areg.R"
is_new_file = true
description = "Add translation support for areg through the shared estimation-side-effects framework."
---

```r
# FILE: R/t_areg.R

t_areg = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_areg")
  t_estimation_cmd(rest_of_cmd, cmd_obj, cmd_df, line_num, context, estimator = "areg")
}
```

!END_MODIFICATION t_areg.R

!MODIFICATION t_xtreg.R
scope = "file"
file = "R/t_xtreg.R"
is_new_file = true
description = "Add translation support for xtreg through the shared estimation-side-effects framework."
---

```r
# FILE: R/t_xtreg.R

t_xtreg = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_xtreg")
  t_estimation_cmd(rest_of_cmd, cmd_obj, cmd_df, line_num, context, estimator = "xtreg")
}
```

!END_MODIFICATION t_xtreg.R

!MODIFICATION t_probit.R
scope = "file"
file = "R/t_probit.R"
is_new_file = true
description = "Add translation support for probit through the shared estimation-side-effects framework."
---

```r
# FILE: R/t_probit.R

t_probit = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_probit")
  t_estimation_cmd(rest_of_cmd, cmd_obj, cmd_df, line_num, context, estimator = "probit")
}
```

!END_MODIFICATION t_probit.R

!MODIFICATION t_reghdfe.R
scope = "file"
file = "R/t_reghdfe.R"
is_new_file = true
description = "Add translation support for reghdfe through the shared estimation-side-effects framework."
---

```r
# FILE: R/t_reghdfe.R

t_reghdfe = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_reghdfe")
  t_estimation_cmd(rest_of_cmd, cmd_obj, cmd_df, line_num, context, estimator = "reghdfe")
}
```

!END_MODIFICATION t_reghdfe.R

!MODIFICATION t_logit.R
scope = "file"
file = "R/t_logit.R"
is_new_file = true
description = "Add translation support for logit through the shared estimation-side-effects framework."
---

```r
# FILE: R/t_logit.R

t_logit = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_logit")
  t_estimation_cmd(rest_of_cmd, cmd_obj, cmd_df, line_num, context, estimator = "logit")
}
```

!END_MODIFICATION t_logit.R

!MODIFICATION t_ivregress.R
scope = "file"
file = "R/t_ivregress.R"
is_new_file = true
description = "Add translation support for ivregress through the shared estimation-side-effects framework."
---

```r
# FILE: R/t_ivregress.R

t_ivregress = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_ivregress")
  t_estimation_cmd(rest_of_cmd, cmd_obj, cmd_df, line_num, context, estimator = "ivregress")
}
```

!END_MODIFICATION t_ivregress.R
