# Translate Stata 'preserve' and 'restore' commands

# Global stack for preserve/restore if multiple levels are needed
# This should ideally be managed by the calling environment of the translated code.
# For now, we generate code that uses variables like `data_preserved_L<line>`

# preserve_stack_name = "stata_preserve_stack_internal" # Name of list in R environment

t_preserve_restore = function(cmd_obj, type = "preserve") { # line_num implicitly from cmd_obj$line
  restore.point("t_preserve_restore") # Added restore.point
  # Preserve: Make a copy of the current 'data' dataframe.
  # Restore: Replace 'data' with the last preserved version.

  # Stata preserve/restore can be nested. A stack is needed.
  # The translated R code will try to mimic this using uniquely named backup variables.
  # A simpler approach if only one level of preserve/restore is common:
  # preserve -> data_backup = data
  # restore -> data = data_backup

  # To handle nesting, we use line numbers to make backup names unique.
  # This assumes preserve/restore are properly paired.
  # A true stack mechanism would require the list `stata_preserve_stack_internal` to be managed.
  # For now, let's use unique variable names based on line number. This is not a stack,
  # it means a `restore` must correspond to a specific `preserve`'s variable. This is not how Stata works.

  r_code_lines = c(
    "if (!exists('stata_data_preserve_stack_G')) stata_data_preserve_stack_G = list() # Global stack for preserve/restore"
  )

  if (type == "preserve") {
    r_code_lines = c(r_code_lines,
      "stata_data_preserve_stack_G = c(list(data), stata_data_preserve_stack_G)",
      paste0("# Preserved data state from Stata line: ", cmd_obj$line)
    )
  } else if (type == "restore") {
    r_code_lines = c(r_code_lines,
      "if (length(stata_data_preserve_stack_G) > 0) {",
      "  data = stata_data_preserve_stack_G[[1]]",
      "  stata_data_preserve_stack_G = stata_data_preserve_stack_G[-1]",
      paste0("  # Restored data state from Stata line: ", cmd_obj$line),
      "} else {",
      paste0("  warning('Stata restore called on line ", cmd_obj$line, " but preserve stack is empty.')"),
      "}"
    )
  }

  return(paste(r_code_lines, collapse = "\n"))
}

