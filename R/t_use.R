# Translate Stata 'use' command
t_use = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  # Example: use "filename.dta", clear
  #          use "`macroname'", clear

  parts = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*(\"[^\"]+\"|`[^']+')\\s*(?:,\\s*(clear))?")
  # Group 1: filename (quoted or macro)
  # Group 2: clear (optional)

  if (is.na(parts[1,1])) {
    return(paste0("# Failed to parse use command: ", rest_of_cmd))
  }

  filename_str = parts[1,2]
  clear_opt = parts[1,3] # NA if not present, "clear" if present

  # Handle macro `filename'
  if (stringi::stri_startswith_fixed(filename_str, "`") && stringi::stri_endswith_fixed(filename_str, "'")) {
    macro_name = stringi::stri_sub(filename_str, 2, -2)
    # We need to find where this macro was defined (e.g. by tempfile)
    # and get the R variable name for the path or dataframe.
    # This requires a robust macro/tempfile tracking system.
    # For now, assume a naming convention like R_tempfile_L<line>_<macroname>_path or R_tempdata_L<line>_<macroname>

    # Scan cmd_df for tempfile definition of this macro
    path_r_var = NA_character_
    data_r_var = NA_character_ # If tempfile implies an R dataframe directly

    for (i in (line_num - 1):1) {
        if (cmd_df$stata_cmd[i] == "tempfile" && grepl(paste0("\\b",macro_name,"\\b"), cmd_df$rest_of_cmd[i])) {
            # Assuming t_tempfile creates R_tempfile_L<line>_<macro>_path for file path
            # and potentially R_tempdata_L<line>_<macro> if it's an R object.
            path_r_var = paste0("R_tempfile_L", cmd_df$line[i], "_", macro_name, "_path")
            data_r_var = paste0("R_tempdata_L", cmd_df$line[i], "_", macro_name) # if save `macroname` means assign to this R df
            break
        }
        # also check if `save "`macroname'"` implied an R dataframe variable
        if (cmd_df$stata_cmd[i] == "save" && grepl(paste0("`",macro_name,"'"), cmd_df$rest_of_cmd[i])) {
             data_r_var = paste0("R_tempdata_L", cmd_df$line[i], "_", macro_name) # As if t_save created this
             # Path might also be associated if save also wrote to disk.
             path_r_var = paste0("R_tempfile_L", cmd_df$line[i], "_", macro_name, "_path") # Convention
             break
        }
    }

    if (!is.na(data_r_var) && !is.na(clear_opt)) { # If `use ... clear` and we have an R object for it
        # Check if data_r_var likely exists (this is a runtime check, hard for translator)
        # For now, assume if tempfile was used, it means an R dataframe object mostly
        return(paste0("data = ", data_r_var))
    } else if (!is.na(path_r_var)) {
        filename_r = path_r_var # Use the R variable holding the path
    } else {
        # Fallback: assume macro is an environment variable or simply translate string
        filename_r = paste0('Sys.getenv("', macro_name, '", unset = ', filename_str, ')') # Or just filename_str
        warning(paste0("Macro ",filename_str, " in 'use' command at line ",line_num, " may not be correctly resolved."))
        filename_r = filename_str # Simplest: use the macro name as string, assume it's defined in R
    }
  } else {
    # Quoted filename
    filename_r = filename_str
  }

  # If `clear` option is present, previous data is cleared.
  # In R, this means simply assigning new data to 'data'.
  # If not `clear` and `data` exists, Stata would error. R `read_dta` just overwrites.

  # Using haven::read_dta
  # Assuming Stata .dta files. If other types, logic needs extension.
  r_code = paste0("data = haven::read_dta(", filename_r, ")")

  # Add a comment about 'clear' if it was used
  if (!is.na(clear_opt)) {
    r_code = paste0(r_code, " # 'clear' was used")
  }

  return(r_code)
}

