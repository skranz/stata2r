


Here are the modifications to adapt 5 Stata commands (`rename`, `order`, `compress`, `sort`, and `save`) into the new 3-function paradigm (`s2r_p_*`, `t_*`, `scmd_*`). 

These changes will standardize parsing, allow variable abbreviations via `expand_varlist` dynamically at runtime, and vastly simplify code generation.

!MODIFICATION t_rename.R
scope = "file"
file = "R/t_rename.R"
is_new_file = false
description = "Rewrite rename translation into parsing, code gen, and runtime execution functions"
---
```r
# FILE: R/t_rename.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_rename = function(rest_of_cmd) {
  restore.point("s2r_p_rename")
  parts = stringi::stri_split_regex(stringi::stri_trim_both(rest_of_cmd), "\\s+", n = 2)[[1]]
  list(
    old_var = if(length(parts) >= 1) parts[1] else NA_character_,
    new_var = if(length(parts) == 2) parts[2] else NA_character_
  )
}

# 2. Code Generation Phase: Emit R code
t_rename = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_rename")
  parsed = s2r_p_rename(rest_of_cmd)
  
  if (is.na(parsed$old_var) || is.na(parsed$new_var)) {
    return(paste0("# Failed to parse rename command: ", rest_of_cmd))
  }

  # Build call to runtime execution function `scmd_rename`
  args = c("data = data",
           paste0("old_var = ", quote_for_r_literal(parsed$old_var)),
           paste0("new_var = ", quote_for_r_literal(parsed$new_var)))

  r_code_str = paste0("data = scmd_rename(", paste(args, collapse = ", "), ")")
  return(r_code_str)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_rename = function(data, old_var, new_var) {
  restore.point("scmd_rename")
  
  # Allow runtime abbreviation for old_var
  expanded_old = expand_varlist(old_var, names(data))
  if (length(expanded_old) == 0) {
    stop(paste0("scmd_rename: variable '", old_var, "' not found"))
  }
  
  old_var_actual = expanded_old[1]
  
  # Base R implementation is fastest and robust here
  names(data)[names(data) == old_var_actual] = new_var
  
  return(data)
}
```
!END_MODIFICATION t_rename.R


!MODIFICATION t_order.R
scope = "file"
file = "R/t_order.R"
is_new_file = false
description = "Rewrite order translation into parsing, code gen, and runtime execution functions"
---
```r
# FILE: R/t_order.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_order = function(rest_of_cmd) {
  restore.point("s2r_p_order")
  parts = stringi::stri_match_first_regex(stringi::stri_trim_both(rest_of_cmd), "^\\s*(.*?)(?:,\\s*(.*))?$")
  list(
    varlist = stringi::stri_trim_both(parts[1,2]),
    options = stringi::stri_trim_both(parts[1,3])
  )
}

# 2. Code Generation Phase: Emit R code
t_order = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_order")
  parsed = s2r_p_order(rest_of_cmd)
  
  if (is.na(parsed$varlist) || parsed$varlist == "") {
    return("# order command with no variables specified.")
  }

  # Build call to runtime execution function `scmd_order`
  args = c("data = data",
           paste0("varlist_str = ", quote_for_r_literal(parsed$varlist)))
  r_code_str = paste0("data = scmd_order(", paste(args, collapse = ", "), ")")

  if (!is.na(parsed$options) && parsed$options != "") {
    r_code_str = paste0(r_code_str, " # Options ignored: ", parsed$options)
  }
  
  # Maintain package internal tracking variables
  if (isTRUE(stata2r_env$has_original_order_idx)) {
    r_code_str = paste0(r_code_str, " %>% \n  dplyr::mutate(stata2r_original_order_idx = dplyr::row_number())")
  }
  
  return(r_code_str)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_order = function(data, varlist_str) {
  restore.point("scmd_order")
  
  # Resolve variable list patterns/abbreviations dynamically
  cols_to_order = expand_varlist(varlist_str, names(data))
  
  if (length(cols_to_order) > 0) {
    other_cols = setdiff(names(data), cols_to_order)
    data = data[, c(cols_to_order, other_cols), drop = FALSE]
  }
  
  return(data)
}
```
!END_MODIFICATION t_order.R


!MODIFICATION t_compress.R
scope = "file"
file = "R/t_compress.R"
is_new_file = false
description = "Rewrite compress translation into parsing, code gen, and runtime execution functions"
---
```r
# FILE: R/t_compress.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_compress = function(rest_of_cmd) {
  restore.point("s2r_p_compress")
  list(varlist = stringi::stri_trim_both(rest_of_cmd))
}

# 2. Code Generation Phase: Emit R code
t_compress = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_compress")
  parsed = s2r_p_compress(rest_of_cmd)

  args = c("data = data")
  if (!is.na(parsed$varlist) && parsed$varlist != "") {
    args = c(args, paste0("varlist_str = ", quote_for_r_literal(parsed$varlist)))
  }

  r_code_str = paste0("data = scmd_compress(", paste(args, collapse = ", "), ")")
  return(r_code_str)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_compress = function(data, varlist_str = NA_character_) {
  restore.point("scmd_compress")
  
  cols_to_compress = names(data)
  if (!is.na(varlist_str) && varlist_str != "") {
    # Dynamically expand variable patterns
    cols_to_compress = expand_varlist(varlist_str, names(data))
  }

  # Apply R implementation for Stata's compress logic
  for (col in cols_to_compress) {
    data[[col]] = sfun_compress_col_type(data[[col]])
  }
  
  return(data)
}
```
!END_MODIFICATION t_compress.R


!MODIFICATION t_sort.R
scope = "file"
file = "R/t_sort.R"
is_new_file = false
description = "Rewrite sort translation into parsing, code gen, and runtime execution functions"
---
```r
# FILE: R/t_sort.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_sort = function(rest_of_cmd) {
  restore.point("s2r_p_sort")
  list(varlist = stringi::stri_trim_both(rest_of_cmd))
}

# 2. Code Generation Phase: Emit R code
t_sort = function(rest_of_cmd, cmd_obj, cmd_df, line_num, type = "sort") {
  restore.point("t_sort")
  parsed = s2r_p_sort(rest_of_cmd)
  
  if (is.na(parsed$varlist) || parsed$varlist == "") {
    return("# sort/gsort command with no variables specified.")
  }

  args = c("data = data",
           paste0("varlist_str = ", quote_for_r_literal(parsed$varlist)),
           paste0("type = ", quote_for_r_literal(type)))
  
  r_code_str = paste0("data = scmd_sort(", paste(args, collapse = ", "), ")")

  # Maintain package internal tracking variables (updating after sort)
  if (isTRUE(stata2r_env$has_original_order_idx)) {
    r_code_str = paste0(r_code_str, " %>% \n  dplyr::mutate(stata2r_original_order_idx = dplyr::row_number())")
  }
  
  return(r_code_str)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_sort = function(data, varlist_str, type = "sort") {
  restore.point("scmd_sort")
  
  tokens = stringi::stri_split_regex(trimws(varlist_str), "\\s+")[[1]]
  tokens = tokens[tokens != ""]
  
  if (length(tokens) == 0) return(data)
  
  sort_exprs = character(0)
  
  # Resolve +/- and wildcards correctly
  for (tok in tokens) {
    desc = FALSE
    if (type == "gsort") {
      if (startsWith(tok, "-")) {
        desc = TRUE
        tok = substring(tok, 2)
      } else if (startsWith(tok, "+")) {
        tok = substring(tok, 2)
      }
    }
    
    expanded = expand_varlist(tok, names(data))
    for (v in expanded) {
      if (desc) {
        sort_exprs = c(sort_exprs, paste0("dplyr::desc(`", v, "`)"))
      } else {
        sort_exprs = c(sort_exprs, paste0("`", v, "`"))
      }
    }
  }
  
  if (length(sort_exprs) > 0) {
    # Dynamically evaluate the correctly expanded and scoped expressions
    cmd = paste0("dplyr::arrange(data, ", paste(sort_exprs, collapse = ", "), ")")
    data = eval(parse(text = cmd))
  }
  
  return(data)
}
```
!END_MODIFICATION t_sort.R


!MODIFICATION t_save.R
scope = "file"
file = "R/t_save.R"
is_new_file = false
description = "Rewrite save translation into parsing, code gen, and runtime execution functions"
---
```r
# FILE: R/t_save.R

# 1. Parsing Phase: Extract Stata syntax components
s2r_p_save = function(rest_of_cmd) {
  restore.point("s2r_p_save")
  parts = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*([^,]*?)(?:,\\s*(.*))?$")
  list(
    raw_filename = stringi::stri_trim_both(parts[1,2]),
    options = stringi::stri_trim_both(parts[1,3])
  )
}

# 2. Code Generation Phase: Emit R code
t_save = function(rest_of_cmd, cmd_obj, cmd_df, line_num) {
  restore.point("t_save")
  parsed = s2r_p_save(rest_of_cmd)

  if (is.na(parsed$raw_filename) || parsed$raw_filename == "") {
    return("# `save` without filename not fully supported yet. Needs to track original data filename.")
  }

  # resolve_stata_filename returns an R expression evaluating to the path dynamically
  filename_r_expr = resolve_stata_filename(parsed$raw_filename, cmd_df, line_num, default_base_dir_var = "working_dir")

  args = c("data = data",
           paste0("file_path = ", filename_r_expr))

  r_code_str = paste0("data = scmd_save(", paste(args, collapse = ", "), ")")
  
  if (!is.na(parsed$options) && parsed$options != "") {
    r_code_str = paste0(r_code_str, " # Options ignored: ", parsed$options)
  }

  return(r_code_str)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_save = function(data, file_path) {
  restore.point("scmd_save")
  haven::write_dta(data, path = file_path)
  
  # Return data unchanged
  return(data)
}
```
!END_MODIFICATION t_save.R
