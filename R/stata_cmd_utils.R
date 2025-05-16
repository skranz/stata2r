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
  "preserve", "recode", "rename", "reshape", "restore", "sample", "save",
  "set", # e.g. set obs, set type (can change data interpretation)
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
  "view", "version", "v", "webuse", "which", "w", "while", "window", "winexec", "xmlsav"
)

# Helper to parse basic Stata command line: cmd + rest
# Tries to handle `by varlist : command` prefix.
parse_stata_command_line = function(line_text) {
  trimmed_line = stringi::stri_trim_both(line_text)

  by_vars = NA_character_
  rest_of_line_for_cmd_parse = trimmed_line
  is_by_prefix_val = FALSE

  # Check for "by ... :" or "bysort ... :" prefix
  if (stringi::stri_startswith_fixed(trimmed_line, "by ") || stringi::stri_startswith_fixed(trimmed_line, "bysort ")) {
    prefix_match = stringi::stri_match_first_regex(trimmed_line, "^(?:by|bysort)\\s+([^:]+?)\\s*:\\s*(.*)$")
    if (!is.na(prefix_match[1,1])) {
      by_vars = stringi::stri_trim_both(prefix_match[1,2])
      # Remove trailing space from by_vars if any from non-greedy match
      by_vars = stringi::stri_trim_both(by_vars)
      rest_of_line_for_cmd_parse = stringi::stri_trim_both(prefix_match[1,3])
      is_by_prefix_val = TRUE # It's a prefix if not bysort command itself
    }
  }

  # Extract command token from the (potentially remaining) line
  # Split only on the first space to separate command from the rest
  parts = stringi::stri_split_fixed(rest_of_line_for_cmd_parse, " ", n = 2)
  cmd_token_original = parts[[1]][1]

  if (is.na(cmd_token_original) || cmd_token_original == "") { # Empty line or only by prefix
      return(list(
        stata_cmd_original = NA_character_,
        stata_cmd = NA_character_,
        rest_of_cmd = NA_character_,
        by_vars = by_vars,
        is_by_prefix = is_by_prefix_val # True if "by prefix:" was found
      ))
  }

  stata_cmd = get_stata_full_cmd_name(cmd_token_original)

  rest_of_cmd = NA_character_
  if (length(parts[[1]]) > 1 && !is.na(parts[[1]][2])) {
    rest_of_cmd = stringi::stri_trim_both(parts[[1]][2])
  }

  # Refine is_by_prefix: it's a prefix if by_vars is set AND the command itself is not 'bysort'
  # because 'bysort' command handles its own by-variables as part of its syntax.
  if (stata_cmd == "bysort") {
      is_by_prefix_val = FALSE # bysort is the command, not a prefix to another command
      # For bysort, by_vars identified by prefix regex are actually part of its command arguments
      # if rest_of_cmd is empty and by_vars were parsed by prefix regex.
      # Example: bysort grp: -> by_vars="grp", cmd="bysort", rest_of_cmd="" is wrong.
      # cmd="bysort", rest_of_cmd="grp" (if that was the structure) or by_vars = NULL if bysort consumes it
      # The regex handles "bysort grp: egen ..." correctly; by_vars="grp", cmd="egen"
      # If line is "bysort grp var", then by_vars=NA, cmd="bysort", rest="grp var"
      # Current logic: if "bysort grp: egen ...", is_by_prefix_val=TRUE, stata_cmd="egen". This is fine.
  } else if (stata_cmd == "by") { # "by" is an alias for "bysort"
      stata_cmd = "bysort"
      is_by_prefix_val = FALSE
  }


  return(list(
    stata_cmd_original = cmd_token_original,
    stata_cmd = stata_cmd,
    rest_of_cmd = rest_of_cmd,
    by_vars = by_vars,
    is_by_prefix = is_by_prefix_val && !is.na(by_vars) # Ensure by_vars is not NA
  ))
}


