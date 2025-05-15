# Stata command utilities

# Main Stata commands and their abbreviations
# This list is not exhaustive but covers many common commands.
stata_cmd_abbreviations = list(
  "a" = "append",
  "br" = "browse",
  "by" = "bysort", # 'by' is often a prefix, but can be 'bysort'
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
  "mark" = "marksample", # typically used with touse
  "markout" = "markout", # typically used with touse
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
  "temp" = "tempfile", # or tempvar, tempname
  "ty" = "type",
  "u" = "use",
  "v" = "version",
  "w" = "which",
  "x" = "xtile"
  # Add more as needed
)

# Function to get the full Stata command name from a token (could be an abbreviation)
get_stata_full_cmd_name = function(cmd_token) {
  cmd_token_lower = tolower(cmd_token)
  if (cmd_token_lower %in% names(stata_cmd_abbreviations)) {
    return(stata_cmd_abbreviations[[cmd_token_lower]])
  }
  # If not in abbreviations, assume it's already a full (or unrecognized) command
  # For recognized full commands, ensure they are in a canonical form (e.g. all lowercase)
  # This step can be enhanced by having a list of all known full commands.
  return(cmd_token_lower)
}


# List of Stata commands considered to modify the dataset or produce results for later modification
# This list helps `mark_data_manip_cmd`.
# It's not exhaustive and might need refinement based on context (e.g. `summarize`).
stata_data_manip_cmds = c(
  "append", "collapse", "compress", "contract", "decode", "destring", "drop",
  "duplicates", "egen", "encode", "expand", "fillin", "format", # format can change how data is used by other commands
  "generate", "gen", "gsort", "input", "insheet", "keep", "label", # label can be part of data definition
  "merge", "modify", "move", "mvdecode", "mvrecode", "order", "pctile",
  "preserve", "recode", "rename", "reshape", "restore", "run", "sample", "save",
  "set", # set type, set obs etc.
  "sort", "stack", "statsby", "stsplit", "summarize", # summarize if r() is used
  "svar", "sysuse", "tempfile", "tempvar", "tempname", # for managing state used by other data ops
  "total", "type", "use", "xtile"
  # Commands like `replace` are handled implicitly if `generate` covers it. `replace` is a variant of `generate`.
  # Explicitly add replace
  ,"replace"
)
# Commands that primarily display info or control program flow, not direct data manip usually
stata_non_data_manip_cmds = c(
  "assert", "browse", "capture", "cd", "clear", "confirm", "constraint", "correlate",
  "count", "describe", "dir", "display", "do", "edit", "erase", "error", "estimates",
  "exit", "findit", "graph", "help", "if", "inspect", "list", "log", "lookup", "marksample",
  "matrix", "memory", "mkdir", "more", "notes", "outfile", "outsheet", "pause", "plot",
  "predict", # predict often generates new variables, so it IS manip. Re-evaluate.
  "print", "program", "pwd", "query", "quietly", "regress", # regress sets e(), may be used.
  "return", "rmdir", "run", "scalar", "search", "shell", "signestim", "sleep",
  "stata", "tabdisp", "table", "tabulate", "test", "timer", "translate", "truncate",
  "tutorials", "view", "version", "webuse", "which", "while", "window", "winexec", "xmlsav"
  # `clear` is an option of `use` or a command. If command, it clears data.
)

# Helper to parse basic Stata command line: cmd + rest
# Tries to handle `by varlist : command` prefix.
parse_stata_command_line = function(line_text) {
  trimmed_line = stringi::stri_trim_both(line_text)

  by_vars = NA_character_
  rest_of_line_for_cmd_parse = trimmed_line

  # Check for "by ... :" prefix
  # Regex: ^by\s+([\w\s]+?)\s*:\s*(.*)
  # \w for var names, \s for multiple var names
  # Needs to be non-greedy for varlist for cases like "by group: by year: command" (not handled now)
  if (stringi::stri_startswith_fixed(trimmed_line, "by ") || stringi::stri_startswith_fixed(trimmed_line, "bysort ")) {
    # A more robust regex to capture varlist until ':'
    # Covers 'by var1 var2:', 'bysort var1 var2:'
    prefix_match = stringi::stri_match_first_regex(trimmed_line, "^(?:by|bysort)\\s+([^:]+):\\s*(.*)$")
    if (!is.na(prefix_match[1,1])) {
      by_vars = stringi::stri_trim_both(prefix_match[1,2])
      rest_of_line_for_cmd_parse = stringi::stri_trim_both(prefix_match[1,3])
    }
  }

  # Extract command token from the (potentially remaining) line
  parts = stringi::stri_split_fixed(rest_of_line_for_cmd_parse, " ", n = 2)
  cmd_token_original = parts[[1]][1]

  if (is.na(cmd_token_original) || cmd_token_original == "") { # Empty line or only by prefix
      return(list(
        stata_cmd_original = NA_character_,
        stata_cmd = NA_character_,
        rest_of_cmd = NA_character_,
        by_vars = by_vars,
        is_by_prefix = !is.na(by_vars)
      ))
  }

  stata_cmd = get_stata_full_cmd_name(cmd_token_original)

  rest_of_cmd = NA_character_
  if (length(parts[[1]]) > 1) {
    rest_of_cmd = stringi::stri_trim_both(parts[[1]][2])
  }

  # Special case: `replace` is not an abbreviation, it's a command like `generate`
  if (tolower(cmd_token_original) == "replace" && stata_cmd != "replace") {
      stata_cmd = "replace"
  }


  return(list(
    stata_cmd_original = cmd_token_original,
    stata_cmd = stata_cmd,
    rest_of_cmd = rest_of_cmd,
    by_vars = by_vars, # comma separated list of by variables
    is_by_prefix = !is.na(by_vars) && stata_cmd != "bysort" # bysort handles its own by_vars
  ))
}


