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

  # Check if it matches a prefix of a known full command,
  # AND is longer than the minimum abbreviation
  for (abbr in names(stata_cmd_abbreviations)) {
    full_cmd = stata_cmd_abbreviations[[abbr]]
    if (startsWith(full_cmd, cmd_token_lower) && startsWith(cmd_token_lower, abbr)) {
      return(full_cmd)
    }
  }

  return(cmd_token_lower)
}

# Vectorized version of get_stata_full_cmd_name
get_stata_full_cmd_name_vec = function(cmd_tokens) {
  restore.point("get_stata_full_cmd_name_vec")

  res = rep(NA_character_, length(cmd_tokens))
  valid_idx = !is.na(cmd_tokens) & cmd_tokens != ""

  if (any(valid_idx)) {
    cmd_tokens_lower = tolower(cmd_tokens[valid_idx])
    res_valid = cmd_tokens_lower

    abbrs = names(stata_cmd_abbreviations)
    full_cmds = unlist(stata_cmd_abbreviations, use.names = FALSE)

    # 1. Exact match in abbreviations
    exact_match = match(cmd_tokens_lower, abbrs)
    has_exact = !is.na(exact_match)
    res_valid[has_exact] = full_cmds[exact_match[has_exact]]

    # 2. Prefix matching for those without exact match
    needs_prefix = !has_exact
    if (any(needs_prefix)) {
      tokens_to_check = cmd_tokens_lower[needs_prefix]
      resolved = tokens_to_check

      for (i in seq_along(tokens_to_check)) {
        tok = tokens_to_check[i]
        for (j in seq_along(abbrs)) {
          abbr = abbrs[j]
          full_cmd = full_cmds[j]
          if (startsWith(full_cmd, tok) && startsWith(tok, abbr)) {
            resolved[i] = full_cmd
            break
          }
        }
      }
      res_valid[needs_prefix] = resolved
    }
    res[valid_idx] = res_valid
  }
  return(res)
}


# List of Stata commands considered to modify the dataset
stata_data_manip_cmds = c(
  "append", "collapse", "compress", "contract", "decode", "destring", "drop",
  "duplicates", "egen", "encode", "expand", "fillin",
  "generate", "gen", "gsort", "input", "insheet", "keep", "label",
  "merge", "modify", "move", "mvdecode", "mvrecode", "order", "pctile",
  "predict",
  "preserve", "recode", "rename", "reshape", "restore", "sample",
  # save will not modify the data set,
  # it is not helpful to include in our pipeline
  # "save",
  "set","sort", "stack", "statsby", "stsplit",
  "svar", "sysuse",
  "tempfile", "tempvar", "tempname",
  "total",
  "use", "xtile", "xi",
  "replace", "clear", "scalar", "sc"
)

# Commands that primarily display info or control program flow, and never
# modify data or produce e()/r() results for later use.
stata_non_data_manip_cmds = c(
  "assert", "browse", "capture", "cd", "confirm", "constraint", "display", "di", "do", "edit", "erase", "error",
  "exit", "findit", "format", "graph", "gr", "help", "h", "if", "inspect", "i", "list", "l", "log", "lookup", "marksample",
  "matrix", "mat", "memory", "mem", "mkdir", "more", "mo", "notes", "n", "outfile", "outsheet", "ou", "pause", "plot",
  "print", "program", "pwd", "query",
  "return", "ret", "rmdir", "run", "ru", "search", "shell", "sh", "signestim", "sleep",
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


# Vectorized helper to parse multiple Stata command lines
parse_stata_command_lines = function(lines_text) {
  restore.point("parse_stata_command_lines")
  n = length(lines_text)
  if (n == 0) {
    return(list(
      stata_cmd_original = character(0),
      stata_cmd = character(0),
      rest_of_cmd = character(0),
      is_by_prefix = logical(0),
      is_bysort_prefix = logical(0),
      by_group_vars = character(0),
      by_sort_vars = character(0),
      is_quietly_prefix = logical(0),
      is_capture_prefix = logical(0),
      is_xi_prefix = logical(0)
    ))
  }

  effective_line = stringi::stri_trim_both(lines_text)

  is_by_prefix_val = rep(FALSE, n)
  is_bysort_prefix_val = rep(FALSE, n)
  by_group_vars = rep("", n)
  by_sort_vars = rep("", n)
  is_quietly_prefix_val = rep(FALSE, n)
  is_capture_prefix_val = rep(FALSE, n)
  is_xi_prefix_val = rep(FALSE, n)

  # 1. capture prefix
  capture_match = stringi::stri_match_first_regex(effective_line, "^(?:capture|cap)\\s+(.*)$")
  idx_cap = !is.na(capture_match[,1])
  if (any(idx_cap)) {
    is_capture_prefix_val[idx_cap] = TRUE
    effective_line[idx_cap] = stringi::stri_trim_both(capture_match[idx_cap, 2])
  }

  # 2. quietly prefix
  quietly_match = stringi::stri_match_first_regex(effective_line, "^(?:quietly|qui|q)\\s+(.*)$")
  idx_qui = !is.na(quietly_match[,1])
  if (any(idx_qui)) {
    is_quietly_prefix_val[idx_qui] = TRUE
    effective_line[idx_qui] = stringi::stri_trim_both(quietly_match[idx_qui, 2])
  }

  # 3. by / bysort prefix
  idx_by_starts = stringi::stri_startswith_fixed(effective_line, "by ") | stringi::stri_startswith_fixed(effective_line, "bysort ")
  if (any(idx_by_starts)) {
    prefix_match = stringi::stri_match_first_regex(effective_line[idx_by_starts], "^(by|bysort)\\s+([^:]+?)\\s*:\\s*(.*)$")
    valid_by = !is.na(prefix_match[,1])
    if (any(valid_by)) {
      idx_valid_by = which(idx_by_starts)[valid_by]
      raw_prefix = stringi::stri_trim_both(prefix_match[valid_by, 2])
      raw_by_string_from_prefix = stringi::stri_trim_both(prefix_match[valid_by, 3])
      effective_line[idx_valid_by] = stringi::stri_trim_both(prefix_match[valid_by, 4])

      is_by_prefix_val[idx_valid_by] = TRUE
      is_bysort_prefix_val[idx_valid_by] = (raw_prefix == "bysort")

      for (i in seq_along(idx_valid_by)) {
        raw_str = raw_by_string_from_prefix[i]
        grp_vars = character(0)
        srt_vars = character(0)

        if (!is.na(raw_str) && raw_str != "") {
          match_result = stringi::stri_match_all_regex(raw_str, "\\s*(\\([^)]+\\)|[^\\s()]+)\\s*")
          if (!is.null(match_result[[1]]) && NROW(match_result[[1]]) > 0) {
            by_tokens = match_result[[1]][,2]
            for (token in by_tokens) {
              if (stringi::stri_startswith_fixed(token, "(") && stringi::stri_endswith_fixed(token, ")")) {
                sort_vars_in_paren = stringi::stri_sub(token, 2, -2)
                srt_vars = c(srt_vars, stringi::stri_split_regex(stringi::stri_trim_both(sort_vars_in_paren), "\\s+")[[1]])
              } else {
                grp_vars = c(grp_vars, token)
              }
            }
          }
        }

        grp_vars = grp_vars[!is.na(grp_vars) & grp_vars != ""]
        srt_vars = srt_vars[!is.na(srt_vars) & srt_vars != ""]

        if (length(grp_vars) > 0) by_group_vars[idx_valid_by[i]] = paste(grp_vars, collapse = ",")
        if (length(srt_vars) > 0) by_sort_vars[idx_valid_by[i]] = paste(srt_vars, collapse = ",")
      }
    }
  }

  # 4. xi: prefix
  xi_match = stringi::stri_match_first_regex(effective_line, "^(?:xi)\\s*:\\s*(.*)$")
  idx_xi = !is.na(xi_match[,1])
  if (any(idx_xi)) {
    is_xi_prefix_val[idx_xi] = TRUE
    effective_line[idx_xi] = stringi::stri_trim_both(xi_match[idx_xi, 2])
  }

  # Extract command token from prefix-stripped line
  parts_mat = stringi::stri_split_fixed(effective_line, " ", n = 2, simplify = NA)

  if (is.matrix(parts_mat)) {
    cmd_token_original = stringi::stri_trim_both(parts_mat[, 1])
    rest_of_cmd = stringi::stri_trim_both(parts_mat[, 2])
  } else {
    # Fallback for empty strings (should not normally trigger if simplify = NA and n >= 1)
    cmd_token_original = stringi::stri_trim_both(parts_mat)
    rest_of_cmd = rep(NA_character_, n)
  }

  idx_empty = is.na(cmd_token_original) | cmd_token_original == ""
  if (any(idx_empty)) {
    cmd_token_original[idx_empty] = NA_character_
  }

  stata_cmd = get_stata_full_cmd_name_vec(cmd_token_original)

  idx_by_cmd = which(!is.na(stata_cmd) & (stata_cmd == "bysort" | stata_cmd == "by"))
  if (length(idx_by_cmd) > 0) {
    is_by_prefix_val[idx_by_cmd] = FALSE
    is_bysort_prefix_val[idx_by_cmd] = FALSE
    by_group_vars[idx_by_cmd] = ""
    by_sort_vars[idx_by_cmd] = ""
    stata_cmd[idx_by_cmd[stata_cmd[idx_by_cmd] == "by"]] = "bysort"
  }

  return(list(
    stata_cmd_original = cmd_token_original,
    stata_cmd = stata_cmd,
    rest_of_cmd = rest_of_cmd,
    is_by_prefix = is_by_prefix_val,
    is_bysort_prefix = is_bysort_prefix_val,
    by_group_vars = by_group_vars,
    by_sort_vars = by_sort_vars,
    is_quietly_prefix = is_quietly_prefix_val,
    is_capture_prefix = is_capture_prefix_val,
    is_xi_prefix = is_xi_prefix_val
  ))
}


# Helper to parse basic Stata command line: cmd + rest
# Handles capture, quietly, by/bysort, and xi: prefixes.
parse_stata_command_line = function(line_text) {
  restore.point("parse_stata_command_line")
  trimmed_line = stringi::stri_trim_both(line_text)

  effective_line = trimmed_line

  is_by_prefix_val = FALSE
  is_bysort_prefix_val = FALSE
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
  if (fast_coalesce(stringi::stri_startswith_fixed(effective_line, "by "), FALSE) ||
      fast_coalesce(stringi::stri_startswith_fixed(effective_line, "bysort "), FALSE)) {
    prefix_match = stringi::stri_match_first_regex(effective_line, "^(by|bysort)\\s+([^:]+?)\\s*:\\s*(.*)$")
    if (!is.na(prefix_match[1,1])) {
      raw_prefix = stringi::stri_trim_both(prefix_match[1,2])
      raw_by_string_from_prefix = stringi::stri_trim_both(prefix_match[1,3])
      effective_line = stringi::stri_trim_both(prefix_match[1,4])
      is_by_prefix_val = TRUE
      is_bysort_prefix_val = identical(raw_prefix, "bysort")

      by_tokens = character(0)
      if (!is.na(raw_by_string_from_prefix) && raw_by_string_from_prefix != "") {
        match_result = stringi::stri_match_all_regex(raw_by_string_from_prefix, "\\s*(\\([^)]+\\)|[^\\s()]+)\\s*")
        if (!is.null(match_result[[1]]) && NROW(match_result[[1]]) > 0) {
          by_tokens = match_result[[1]][,2]
        }
      }

      for (token in by_tokens) {
        if (fast_coalesce(stringi::stri_startswith_fixed(token, "(") && stringi::stri_endswith_fixed(token, ")"), FALSE)) {
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
      is_bysort_prefix = is_bysort_prefix_val,
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
    is_bysort_prefix_val = FALSE
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
    is_bysort_prefix = is_bysort_prefix_val,
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
  if (fast_coalesce(stringi::stri_startswith_fixed(s, "\"") && stringi::stri_endswith_fixed(s, "\""), FALSE)) {
    return(stringi::stri_sub(s, 2, -2))
  }
  if (fast_coalesce(stringi::stri_startswith_fixed(s, "'") && stringi::stri_endswith_fixed(s, "'"), FALSE)) {
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

  if (fast_coalesce(stringi::stri_startswith_fixed(unquoted_content, "`") && stringi::stri_endswith_fixed(unquoted_content, "'"), FALSE)) {
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
    is_absolute_path = fast_coalesce(stringi::stri_startswith_fixed(unquoted_content, "/"), FALSE) ||
      fast_coalesce(stringi::stri_detect_regex(unquoted_content, "^[A-Za-z]:[\\\\/]"), FALSE)

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
