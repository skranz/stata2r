# Translate Stata 'reshape' command
# Stata: reshape wide/long stubnames, i(varlist) [j(varname) | j(varname) string] [options]
# This is a complex command. Basic translation using tidyr.

t_reshape = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  rest_of_cmd_trimmed = stringi::stri_trim_both(rest_of_cmd)

  # Parse wide/long, stubnames, options
  # Pattern: ^\s*(wide|long)\s+(.*?)(?:,\\s*(.*))?$
  # G1: type (wide/long), G2: stubnames/varlist, G3: options

  reshape_match = stringi::stri_match_first_regex(rest_of_cmd_trimmed, "^\\s*(wide|long)\\s+(.*?)(?:,\\s*(.*))?$")

  if (is.na(reshape_match[1,1])) {
    return(paste0("# Failed to parse reshape command: ", rest_of_cmd))
  }

  reshape_type = reshape_match[1,2]
  stubnames_or_varlist_str = stringi::stri_trim_both(reshape_match[1,3])
  options_str = stringi::stri_trim_both(reshape_match[1,4]) # NA if no options

  stubnames_or_varlist = stringi::stri_split_regex(stubnames_or_varlist_str, "\\s+")[[1]]
  stubnames_or_varlist = stubnames_or_varlist[stubnames_or_varlist != ""]
  if (length(stubnames_or_varlist) == 0) {
       return(paste0("# reshape command requires stubnames or varlist: ", rest_of_cmd))
  }


  # Parse required options: i() and j()
  i_vars = NA_character_
  j_var = NA_character_
  j_is_string = FALSE # Stata j() can create numeric or string j variable

  if (!is.na(options_str)) {
      i_opt_match = stringi::stri_match_first_regex(options_str, "\\bi\\s*\\(([^)]+)\\)")
      if (!is.na(i_opt_match[1,1])) {
           i_vars = stringi::stri_trim_both(i_opt_match[1,2])
      }

      j_opt_match = stringi::stri_match_first_regex(options_str, "\\bj\\s*\\(([^)]+)\\)")
      if (!is.na(j_opt_match[1,1])) {
           j_part = stringi::stri_trim_both(j_opt_match[1,2])
           # Check if j part contains 'string' option
           j_string_match = stringi::stri_match_first_regex(j_part, "^\\s*([a-zA-Z_][a-zA-Z0-9_]*)(?:\\s+string)?$")
           if (!is.na(j_string_match[1,1])) {
               j_var = j_string_match[1,2]
               j_is_string = !is.na(stringi::stri_match_first_regex(j_part, "\\s+string$")[1,1])
           }
      }
  }

  if (is.na(i_vars) || i_vars == "") {
      return(paste0("# reshape command requires i() variable(s): ", rest_of_cmd))
  }
  i_vars_list = stringi::stri_split_regex(i_vars, "\\s+")[[1]]
  i_vars_list = i_vars_list[i_vars_list != ""]
  i_vars_r_vec_str = paste0('c("', paste(i_vars_list, collapse = '", "'), '")')

  if (is.na(j_var) || j_var == "") {
       # j() is required unless the data is already structured s.t. it's obvious.
       # Stata infers j if possible, but usually j() is explicitly required.
       # For translation, let's assume j() is required.
       return(paste0("# reshape command requires j() variable: ", rest_of_cmd))
  }


  r_code_str = ""

  if (reshape_type == "wide") {
      # Stata `reshape wide stubnames, i(i) j(j)`
      # R `tidyr::pivot_wider(data, id_cols = i_vars, names_from = j_var, values_from = stubnames)`
      # Multiple stubnames means values_from is a vector of stubnames.
      # If stubnames are `inc limit`, R columns become inc_1990, inc_1991, limit_1990, limit_1991
      # Stata by default creates `stubnamejvalue` columns.
      # tidyr default is `stubname_jvalue`. Can control with `names_sep` or `names_from` + `values_from`.
      # Let's use default `names_sep = "_"` for now.

      stubnames_r_vec_str = paste0('c("', paste(stubnames_or_varlist, collapse = '", "'), '")')

      r_code_str = paste0("data = tidyr::pivot_wider(data, id_cols = dplyr::all_of(", i_vars_r_vec_str, "), names_from = ", j_var, ", values_from = dplyr::all_of(", stubnames_r_vec_str, "))")

  } else if (reshape_type == "long") {
      # Stata `reshape long stubnames, i(i) j(jname)`
      # R `tidyr::pivot_longer(data, cols = c(list of stubnames), names_to = jname, values_to = value_name)`
      # Stata: `reshape long inc limit, i(id) j(year)` -> creates `id`, `year`, `inc`, `limit`
      # tidyr: `pivot_longer(..., names_to = "year", values_to = "value")` default puts all values in 'value'.
      # If multiple stubnames, tidyr can gather them separately.
      # `pivot_longer(..., names_to = c(".value", "year"), names_sep = "_")` if original cols were `inc_1990`, `limit_1990`, etc.

      # The `stubnames_or_varlist` for long format lists the *original* wide variables which become the stubs.
      # Example: `reshape long inc90 inc91 limit90 limit91, i(id) j(year)`
      # Stubnames are `inc` and `limit`. The suffix is `90`, `91`.
      # tidyr needs to know the columns to gather: `c(inc90, inc91, limit90, limit91)`
      # Stata infers the stubs and suffixes. We need to parse the original wide columns.
      # This requires analyzing the `stubnames_or_varlist` and potentially options like `string`.

      # Simple approach: Assume stubnames_or_varlist lists all wide columns to gather.
      # e.g. `reshape long inc90 inc91, i(id) j(year)`
      # cols = c(inc90, inc91).
      # names_to = "year" (this is the j variable name)
      # values_to = ".value" (this tells pivot_longer to use the original column names as value columns)
      # names_pattern = "stub(.*)" captures the part that becomes the j variable value.
      # If stubnames are `inc` and `limit`, and columns are `inc_1990`, `inc_1991`, `limit_1990`, `limit_1991`
      # `pivot_longer(cols = starts_with("inc_") | starts_with("limit_"), names_to = c(".value", j_var), names_sep = "_", values_to = "???")`
      # This mapping is tricky.

      # Let's assume the simplest case first: one set of stubnames.
      # `reshape long inc, i(id) j(year)` means original vars are inc1990, inc1991, etc.
      # stubname="inc", jname="year". Columns to gather are "inc*".
      # `tidyr::pivot_longer(data, cols = starts_with(stubname), names_to = jname, values_pattern = "stubname(.*)", values_to = stubname)`
      # Example: `reshape long inc, i(id) j(year)`
      # `stubnames_or_varlist` = "inc". Let's assume this is the single stubname.
      # The original wide columns implicitly match this stub (e.g., inc1990, inc1991).
      # `cols` should match `inc*`. `names_to` is `j_var`. `values_to` is `inc`. `names_pattern` captures the part after "inc".

      if (length(stubnames_or_varlist) > 1) {
           return(paste0("# reshape long with multiple stubnames not yet fully implemented: ", rest_of_cmd))
      }
      stubname = stubnames_or_varlist[1]
      # Assuming wide variables match `stubname + suffix`
      names_pattern = paste0(stubname, "(.*)") # Capture suffix as j variable value
      # If j is string, the captured part is the string. If numeric, need conversion.
      # Stata j() string means the j variable is stored as string.
      # tidyr names_to + names_pattern creates the j variable value.
      # values_to is the name of the variable that holds the actual data values. Stata uses the stubname.

      r_code_str = paste0("data = tidyr::pivot_longer(data, cols = starts_with(\"", stubname, "\"), names_to = \"", j_var, "\", names_pattern = \"", names_pattern, "\", values_to = \"", stubname, "\")")

      # If j() string option was used, need to convert the resulting j_var to string/factor later if tidyr made it numeric.
      # tidyr's names_pattern captures as string by default. If j_is_string is FALSE, need conversion to numeric.
      if (!j_is_string) {
         # Convert j_var to numeric after reshape
         r_code_str = paste0(r_code_str, " %>%\n  dplyr::mutate(", j_var, " = as.numeric(", j_var, "))")
      }

  } else {
      r_code_str = paste0("# Unknown reshape type: ", reshape_type)
  }

  # Add comment about options if any were present but not handled
  if (!is.na(options_str) && !grepl("\\bi\\s*\\([^)]+\\)", options_str) && !grepl("\\bj\\s*\\([^)]+\\)", options_str)) {
       r_code_str = paste0(r_code_str, paste0(" # Other options ignored: ", options_str))
  }


  return(r_code_str)
}

