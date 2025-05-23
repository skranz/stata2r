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
  "q" = "quietly", # Prefix, handled differently
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
  "w" = "which" # find file
  # "x" = "xtile" # egen function typically
)

# Function to get the full Stata command name from a token (could be an abbreviation)
get_stata_full_cmd_name = function(cmd_token) {
  cmd_token_lower = tolower(cmd_token)
  if (cmd_token_lower %in% names(stata_cmd_abbreviations)) {
    return(stata_cmd_abbreviations[[cmd_token_lower]])
  }
  # If not in abbreviations, assume it's already a full (or unrecognized) command
  return(cmd_token_lower)
}


# List of Stata commands considered to modify the dataset or produce results for later modification
stata_data_manip_cmds = c(
  "append", "collapse", "compress", "contract", "decode", "destring", "drop",
  "duplicates", "egen", "encode", "expand", "fillin", "format",
  "generate", "gen", "gsort", "input", "insheet", "keep", "label",
  "merge", "modify", "move", "mvdecode", "mvrecode", "order", "pctile", # pctile often part of egen
  "predict", # predict generates new variables
  "preserve", "recode", "rename", "reshape", "restore", "sample", "set", # e.g. set obs, set type (can change data interpretation)
  "sort", "stack", "statsby", "stsplit",
  "summarize", "su", # summarize if r() is used, or by default include
  "svar", "sysuse", "tabulate", # tabulate can set r() values
  "tempfile", "tempvar", "tempname",
  "total", "type", # type can be for var type changes
  "use", "xtile" # xtile often part of egen
  ,"replace", "clear" # clear (all data), replace
)
# Commands that primarily display info or control program flow, not direct data manip usually
stata_non_data_manip_cmds = c( # This list is for marking FALSE explicitly if needed
  "assert", "browse", "capture", "cd", "confirm", "constraint", "correlate", # correlate sets r() but often for display
  "count", # count sets r() but often for display
  "describe", "d", "dir", "display", "di", "do", "edit", "erase", "error", "estimates",
  "exit", "findit", "graph", "gr", "help", "h", "if", "inspect", "i", "list", "l", "log", "lookup", "marksample",
  "matrix", "mat", "memory", "mem", "mkdir", "more", "mo", "notes", "n", "outfile", "outsheet", "ou", "pause", "plot",
  "print", "program", "pwd", "query", "quietly", "regress", "reg", # regress sets e(), might be used.
  "return", "ret", "rmdir", "run", "ru", "scalar", "sc", "search", "shell", "sh", "signestim", "sleep",
  "stata", "st", "tabdisp", "table", "test", "te", "timer", "translate", "truncate",
  "tutorials", "type", "ty", # `type` command to display file content (different from `set type`)
  "view", "version", "v", "webuse", "w", "which", "while", "window", "winexec", "xmlsav"
)

# Helper to parse basic Stata command line: cmd + rest
# Tries to handle `by varlist : command` prefix.
# Returns:
#   stata_cmd_original: original command token
#   stata_cmd: full command name
#   rest_of_cmd: string after command token (excluding by prefix part)
#   is_by_prefix: logical, TRUE if "by/bysort prefix:" was found
#   by_group_vars: character vector of grouping variables from by/bysort prefix
#   by_sort_vars: character vector of sort-only variables (in parentheses) from by/bysort prefix
parse_stata_command_line = function(line_text) {
  trimmed_line = stringi::stri_trim_both(line_text)

  is_by_prefix_val = FALSE
  by_group_vars = character(0)
  by_sort_vars = character(0)
  raw_by_string_from_prefix = NA_character_
  rest_of_line_for_cmd_parse = trimmed_line

  # Check for "by ... :" or "bysort ... :" prefix
  if (stringi::stri_startswith_fixed(trimmed_line, "by ") || stringi::stri_startswith_fixed(trimmed_line, "bysort ")) {
    prefix_match = stringi::stri_match_first_regex(trimmed_line, "^(?:by|bysort)\\s+([^:]+?)\\s*:\\s*(.*)$")
    if (!is.na(prefix_match[1,1])) {
      raw_by_string_from_prefix = stringi::stri_trim_both(prefix_match[1,2])
      rest_of_line_for_cmd_parse = stringi::stri_trim_both(prefix_match[1,3])
      is_by_prefix_val = TRUE

      # Parse raw_by_string_from_prefix into group_vars and sort_vars
      # Sort vars are in parentheses, e.g., bysort grp (s1 s2):
      # Use regex to find all parenthesized parts and non-parenthesized parts
      by_tokens = stringi::stri_match_all_regex(raw_by_string_from_prefix, "\\s*(\\([^)]+\\)|[^\\s()]+)\\s*")[[1]][,2]

      for (token in by_tokens) {
        if (stringi::stri_startswith_fixed(token, "(") && stringi::stri_endswith_fixed(token, ")")) {
          sort_vars_in_paren = stringi::stri_sub(token, 2, -2)
          by_sort_vars = c(by_sort_vars, stringi::stri_split_regex(stringi::stri_trim_both(sort_vars_in_paren), "\\s+")[[1]])
        } else {
          by_group_vars = c(by_group_vars, token)
        }
      }
      by_group_vars = by_group_vars[by_group_vars != ""]
      by_sort_vars = by_sort_vars[by_sort_vars != ""]
    }
  }

  # Extract command token from the (potentially remaining) line
  parts = stringi::stri_split_fixed(rest_of_line_for_cmd_parse, " ", n = 2)
  cmd_token_original = parts[[1]][1]

  if (is.na(cmd_token_original) || cmd_token_original == "") {
      return(list(
        stata_cmd_original = NA_character_,
        stata_cmd = NA_character_,
        rest_of_cmd = NA_character_,
        is_by_prefix = is_by_prefix_val,
        by_group_vars = if(length(by_group_vars)>0) by_group_vars else NA_character_,
        by_sort_vars = if(length(by_sort_vars)>0) by_sort_vars else NA_character_
      ))
  }

  stata_cmd = get_stata_full_cmd_name(cmd_token_original)
  rest_of_cmd = if (length(parts[[1]]) > 1 && !is.na(parts[[1]][2])) stringi::stri_trim_both(parts[[1]][2]) else NA_character_

  # Refine is_by_prefix: it's a prefix if by_vars were parsed AND command is not 'bysort'
  if (stata_cmd == "bysort" || stata_cmd == "by") { # by is alias for bysort
      is_by_prefix_val = FALSE
      # For bysort command itself, its arguments are in rest_of_cmd.
      # The prefix parsing for by_group_vars/by_sort_vars should be cleared if it's the bysort command.
      by_group_vars = character(0)
      by_sort_vars = character(0)
      if (stata_cmd == "by") stata_cmd = "bysort" # Normalize "by" command to "bysort"
  }

  return(list(
    stata_cmd_original = cmd_token_original,
    stata_cmd = stata_cmd,
    rest_of_cmd = rest_of_cmd,
    is_by_prefix = is_by_prefix_val, # True if "by prefix:" was found AND command is not bysort
    by_group_vars = if(length(by_group_vars)>0) by_group_vars else NA_character_,
    by_sort_vars = if(length(by_sort_vars)>0) by_sort_vars else NA_character_
  ))
}

# Helper function to get macro names from a tempfile command's rest_of_cmd
get_tempfile_macros = function(rest_of_cmd_for_tempfile) {
    if (is.na(rest_of_cmd_for_tempfile) || rest_of_cmd_for_tempfile == "") return(character(0))
    stringi::stri_split_regex(rest_of_cmd_for_tempfile, "\\s+")[[1]] %>%
        stringi::stri_trim_both() %>%
        .[. != ""]
}

# Helper function to unquote Stata string literals or extract macro names
unquote_stata_string_or_macro_literal = function(s) {
  if (is.na(s) || s == "") return(s)

  # First, try to remove outer double quotes
  if (stringi::stri_startswith_fixed(s, '"') && stringi::stri_endswith_fixed(s, '"')) {
    s = stringi::stri_sub(s, 2, -2)
  }
  # Then, try to remove outer single quotes (less common for paths, but possible)
  if (stringi::stri_startswith_fixed(s, "'") && stringi::stri_endswith_fixed(s, "'")) {
    s = stringi::stri_sub(s, 2, -2)
  }

  # Now, check if the remaining string is a Stata local macro: `my_macro'
  if (stringi::stri_startswith_fixed(s, "`") && stringi::stri_endswith_fixed(s, "'")) {
    return(stringi::stri_sub(s, 2, -2))
  }
  # Otherwise, return as is (it's a literal path without quotes, or a variable name)
  return(s)
}

# Helper function to ensure a string is quoted for R literal use if not already
# This function expects an already UNQUOTED string (no Stata-style quotes)
quote_for_r_literal = function(s) {
  if (is.na(s)) return("NA_character_")
  if (s == "") return('""')
  # Check if already quoted with " or '
  if (stringi::stri_startswith_fixed(s, '"') && stringi::stri_endswith_fixed(s, '"')) return(s)
  if (stringi::stri_startswith_fixed(s, "'") && stringi::stri_endswith_fixed(s, "'")) return(s)
  # Add double quotes
  paste0('"', s, '"')
}


