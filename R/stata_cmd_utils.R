# Stata command utilities

# Main Stata commands and their abbreviations
# This list is not exhaustive but covers many common commands.
stata_cmd_abbreviations = list(
  "a" = "append",
  "br" = "browse",
  "by" = "bysort", # 'by' is often a prefix, but can be 'bysort'
  "cap" = "capture",
  "cd" = "cd",
  "cl" = "clear", # clear all
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
  "est" = "estimates", # estimates command group
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
  "logi" = "logit", # Example statistical procedure
  "m" = "merge",
  "mark" = "marksample",
  "markout" = "markout",
  "mat" = "matrix",
  "mem" = "memory",
  "mkdir" = "mkdir",
  "mo" = "more",
  "mov" = "move",
  "mv" = "mvdecode", # mvdecode is a command
  "n" = "notes",
  "o" = "order",
  "ou" = "outsheet",
  "p" = "predict", # predict for generating variables from models
  "pres" = "preserve",
  # "q" = "quietly", # Quietly is a prefix, handled differently
  "r" = "recode",
  "reg" = "regress", # Example statistical procedure
  "ren" = "rename",
  "res" = "reshape",
  "rest" = "restore",
  "ret" = "return", # For return values from programs
  "rm" = "rmdir",
  "ru" = "run", # alias for do
  "sa" = "save",
  "sc" = "scalar",
  "se" = "set", # set memory, set type, etc.
  "sh" = "shell",
  "sig" = "signestim",
  "so" = "sort",
  "st" = "stata", # invoke Stata
  "su" = "summarize",
  "sy" = "sysuse",
  "t" = "tabulate", # tabulate can set r() values
  "te" = "test", # For hypotheses tests
  "temp" = "tempfile", # tempfile, tempvar, tempname
  "ty" = "type", # list content of a file
  "u" = "use",
  "v" = "version", # version control
  "w" = "which", # find file
  "xi" = "xi" # For indicator variable generation
  # "x" = "xtile" # egen function typically
)

# Function to get the full Stata command name from a token (could be an abbreviation)
get_stata_full_cmd_name = function(cmd_token) {
  restore.point("get_stata_full_cmd_name")
  if (is.na(cmd_token) || cmd_token == "") { # Defensive check
    return(NA_character_)
  }
  cmd_token_lower = tolower(cmd_token)
  # Use isTRUE to handle potential NA from %in% (though unlikely for this context)
  if (isTRUE(cmd_token_lower %in% names(stata_cmd_abbreviations))) {
    return(stata_cmd_abbreviations[[cmd_token_lower]])
  }
  # If not in abbreviations, assume it's already a full (or unrecognized) command
  return(cmd_token_lower)
}


# List of Stata commands considered to modify the dataset or produce results for later modification
stata_data_manip_cmds = c(
  "append", "collapse", "compress", "contract", "decode", "destring", "drop",
  "duplicates", "egen", "encode", "expand", "fillin",
  "generate", "gen", "gsort", "input", "insheet", "keep", "label",
  "merge", "modify", "move", "mvdecode", "mvrecode", "order", "pctile", # pctile often part of egen
  "predict", # predict generates new variables
  "preserve", "recode", "rename", "reshape", "restore", "sample", "set", # e.g. set obs, set type (can change data interpretation)
  "sort", "stack", "statsby", "stsplit",
  "svar", "sysuse",
  "tempfile", "tempvar", "tempname",
  "total", # total can generate new variables
  "use", "xtile", # xtile often part of egen
  "replace", "clear", # clear (all data), replace
  "xi" # xi generates new indicator variables
)
# Commands that primarily display info or control program flow, not direct data manip usually
stata_non_data_manip_cmds = c( # This list is for marking FALSE explicitly if needed
  "assert", "browse", "capture", "cd", "confirm", "constraint", "correlate", # correlate sets r() but often for display
  "count", # count sets r() but often for display
  "describe", "d", "dir", "display", "di", "do", "edit", "erase", "error", "estimates", # estimates command group
  "exit", "findit", "format", # Format only affects display, not data values.
  "graph", "gr", "help", "h", "if", "inspect", "i", "list", "l", "log", "lookup", "marksample",
  "matrix", "mat", "memory", "mem", "mkdir", "more", "mo", "notes", "n", "outfile", "outsheet", "ou", "pause", "plot",
  "print", "program", "pwd", "query", # "quietly" is not a command itself, but a prefix
  "return", "ret", "rmdir", "run", "ru", "scalar", "sc", "search", "shell", "sh", "signestim", "sleep",
  "stata", "st", "summarize", "su", # summarize if r() is used, or by default include
  "tabulate", "t", # tabulate can set r() values
  "tabdisp", "table", "test", "te", "timer", "translate", "truncate",
  "tutorials", "type", "ty", # `type` command to display file content (different from `set type`)
  "view", "version", "v", "webuse", "w", "which", "while", "window", "winexec", "xmlsav"
)

# List of estimation commands that can produce e() results
stata_estimation_cmds = c(
  "regress", "logit", "probit", "ivregress", "xtreg", "areg", "sem", "asmixlogit", "gmm" # Add more as needed
  # Could also include ANOVA, factor, etc.
)


# Helper to parse basic Stata command line: cmd + rest
# Tries to handle `by varlist : command` and `quietly command` prefixes.
# Returns:
#   stata_cmd_original: original first token of the effective command (e.g. "g" for "gen")
#   stata_cmd: full command name of the effective command (e.g. "generate")
#   rest_of_cmd: string after effective command token
#   is_by_prefix: logical, TRUE if "by/bysort prefix:" was found
#   by_group_vars: character vector of grouping variables from by/bysort prefix
#   by_sort_vars: character vector of sort-only variables (in parentheses) from by/bysort prefix
#   is_quietly_prefix: logical, TRUE if "quietly" prefix was found
parse_stata_command_line = function(line_text) {
  restore.point("parse_stata_command_line")
  trimmed_line = stringi::stri_trim_both(line_text)

  is_by_prefix_val = FALSE
  by_group_vars = character(0)
  by_sort_vars = character(0)
  raw_by_string_from_prefix = NA_character_
  rest_of_line_for_cmd_parse = trimmed_line

  # Check for "by ... :" or "bysort ... :" prefix
  if (dplyr::coalesce(stringi::stri_startswith_fixed(trimmed_line, "by "), FALSE) ||
      dplyr::coalesce(stringi::stri_startswith_fixed(trimmed_line, "bysort "), FALSE)) {
    prefix_match = stringi::stri_match_first_regex(trimmed_line, "^(?:by|bysort)\\s+([^:]+?)\\s*:\\s*(.*)$")
    if (!is.na(prefix_match[1,1])) {
      raw_by_string_from_prefix = stringi::stri_trim_both(prefix_match[1,2])
      rest_of_line_for_cmd_parse = stringi::stri_trim_both(prefix_match[1,3])
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

  # Check for "quietly" prefix on the (potentially by-prefixed) command part
  is_quietly_prefix_val = FALSE
  # Regex to match "quietly" or "qui" or "q" as a whole word prefix followed by space
  quietly_prefix_regex = "^(?:quietly|qui|q)\\s+(.*)$"
  quietly_match = stringi::stri_match_first_regex(rest_of_line_for_cmd_parse, quietly_prefix_regex)

  if (!is.na(quietly_match[1,1])) {
      # Check if the matched prefix is indeed "quietly" or its abbreviation
      # and not just a command starting with "q" that isn't "quietly" (e.g. "query").
      # The first token before the space:
      first_token_before_space = stringi::stri_extract_first_words(rest_of_line_for_cmd_parse)
      if (tolower(first_token_before_space) %in% c("quietly", "qui", "q")) {
        is_quietly_prefix_val = TRUE
        rest_of_line_for_cmd_parse = stringi::stri_trim_both(quietly_match[1,2]) # The part after "quietly "
      }
  }


  # Extract command token from the (now prefix-stripped) line
  parts = stringi::stri_split_fixed(rest_of_line_for_cmd_parse, " ", n = 2)
  cmd_token_original = stringi::stri_trim_both(parts[[1]][1])

  if (is.na(cmd_token_original) || cmd_token_original == "") {
      return(list(
        stata_cmd_original = NA_character_,
        stata_cmd = NA_character_,
        rest_of_cmd = NA_character_,
        is_by_prefix = is_by_prefix_val,
        by_group_vars = character(0),
        by_sort_vars = character(0),
        is_quietly_prefix = is_quietly_prefix_val # Added
      ))
  }

  stata_cmd = get_stata_full_cmd_name(cmd_token_original)
  rest_of_cmd = if (length(parts[[1]]) > 1 && !is.na(parts[[1]][2])) stringi::stri_trim_both(parts[[1]][2]) else NA_character_

  # Refine is_by_prefix: it's a prefix if by_vars were parsed AND command is not 'bysort'
  if (stata_cmd == "bysort" || stata_cmd == "by") {
      is_by_prefix_val = FALSE
      by_group_vars = character(0)
      by_sort_vars = character(0)
      if (stata_cmd == "by") stata_cmd = "bysort"
  }

  return(list(
    stata_cmd_original = cmd_token_original, # First token of effective command
    stata_cmd = stata_cmd,                   # Full name of effective command
    rest_of_cmd = rest_of_cmd,
    is_by_prefix = is_by_prefix_val,
    by_group_vars = by_group_vars,
    by_sort_vars = by_sort_vars,
    is_quietly_prefix = is_quietly_prefix_val # Added
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
  # Remove outer double quotes
  if (dplyr::coalesce(stringi::stri_startswith_fixed(s, '"') && stringi::stri_endswith_fixed(s, '"'), FALSE)) {
    return(stringi::stri_sub(s, 2, -2))
  }
  # Remove outer single quotes
  if (dplyr::coalesce(stringi::stri_startswith_fixed(s, "'") && stringi::stri_endswith_fixed(s, "'"), FALSE)) {
    return(stringi::stri_sub(s, 2, -2))
  }
  # If not quoted, return as is
  return(s)
}

# Helper function to ensure a string is quoted for R literal use if not already
# This function expects an already UNQUOTED string (no Stata-style quotes)
quote_for_r_literal = function(s) {
  restore.point("quote_for_r_literal")
  if (is.na(s)) return("NA_character_")
  if (s == "") return('""')
  # Check if already quoted with " or '
  if (dplyr::coalesce(stringi::stri_startswith_fixed(s, '"') && stringi::stri_endswith_fixed(s, '"'), FALSE)) return(s)
  if (dplyr::coalesce(stringi::stri_startswith_fixed(s, "'") && stringi::stri_endswith_fixed(s, "'"), FALSE)) return(s)
  # Add double quotes
  paste0('"', s, '"')
}

# Helper function to resolve Stata filenames (literal or macro) to R path expressions
# default_base_dir_var: name of the variable in stata2r_env (e.g., "working_dir", "data_dir")
resolve_stata_filename = function(raw_filename_token, cmd_df, line_num, default_base_dir_var = "working_dir") {
  restore.point("resolve_stata_filename")
  unquoted_content = unquote_stata_string_literal(raw_filename_token)

  # Check if it's a Stata local macro reference `macroname'`
  if (dplyr::coalesce(stringi::stri_startswith_fixed(unquoted_content, "`") && stringi::stri_endswith_fixed(unquoted_content, "'"), FALSE)) {
    macro_name = stringi::stri_sub(unquoted_content, 2, -2)

    found_def_line = NA_integer_
    for (i in (line_num - 1):1) {
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
    # It's a regular path string
    is_absolute_path = dplyr::coalesce(stringi::stri_startswith_fixed(unquoted_content, "/"), FALSE) ||
                       dplyr::coalesce(stringi::stri_detect_regex(unquoted_content, "^[A-Za-z]:[\\\\/]"), FALSE)

    if (is_absolute_path) {
      return(quote_for_r_literal(unquoted_content))
    } else {
      return(paste0("file.path(stata2r_env$", default_base_dir_var, ", ", quote_for_r_literal(unquoted_content), ")"))
    }
  }
}

