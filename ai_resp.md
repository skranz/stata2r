


You make a very good point. **Nothing is lost, and a lot of flexibility is gained.**

Separating the logic into three distinct layers—(1) parsing syntax, (2) translating expressions/emitting code, and (3) runtime execution—is a classic and highly robust compiler design pattern.

Having a standalone `sparse_keep` function means you can easily unit test the Stata regex parser without having to evaluate the generated R code. Furthermore, if you later want to build a static analyzer (e.g., to figure out which variables are dropped/kept before running the script), you can just call `sparse_keep` to get the AST (Abstract Syntax Tree) list.

Here is the updated implementation incorporating the 3-function pattern for `t_keep.R`.

***

Refactored `t_keep.R` to explicitly separate parsing (`sparse_keep`), translation/emission (`t_keep`), and runtime execution (`scmd_keep`).

!CHANGE_FILE R/t_keep.R
# Translate Stata 'keep' command using a 3-tier architecture: Parse, Translate, Execute

# 1. Parsing Phase: Extract Stata syntax components into a structured list
sparse_keep = function(rest_of_cmd) {
  res = list(
    varlist = NA_character_,
    if_str = NA_character_,
    in_str = NA_character_
  )

  str = stringi::stri_trim_both(rest_of_cmd)
  if (is.na(str) || str == "") return(res)

  # Extract `in` condition (must come from the end to avoid matching within strings, though robust parsing would use placeholders)
  in_match = stringi::stri_match_last_regex(str, "\\s+in\\s+(.*)$")
  if (!is.na(in_match[1,1])) {
    res$in_str = stringi::stri_trim_both(in_match[1,2])
    str = stringi::stri_sub(str, 1, in_match[1,1, MRANGE_START=TRUE] - 1)
  }

  # Extract `if` condition
  if_match = stringi::stri_match_last_regex(str, "\\s+if\\s+(.*)$")
  if (!is.na(if_match[1,1])) {
    res$if_str = stringi::stri_trim_both(if_match[1,2])
    str = stringi::stri_sub(str, 1, if_match[1,1, MRANGE_START=TRUE] - 1)
  }

  # Remainder is the varlist
  varlist_str = stringi::stri_trim_both(str)
  if (varlist_str != "") {
    res$varlist = varlist_str
  }

  return(res)
}

# 2. Code Generation Phase: Translate expressions and emit R code
t_keep = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_keep")

  # Parse the Stata command
  parsed = sparse_keep(rest_of_cmd)

  # Translate if expression to R
  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) {
    r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, context)
  }

  # Translate in expression to R numeric range
  r_in_range = NA_character_
  if (!is.na(parsed$in_str)) {
    range_match = stringi::stri_match_first_regex(parsed$in_str, "^(\\d+)(?:/(\\d+))?$")
    if (!is.na(range_match[1,1])) {
      start_row = range_match[1,2]
      end_row = range_match[1,3]
      if (is.na(end_row)) {
        r_in_range = start_row
      } else {
        r_in_range = paste0(start_row, ":", end_row)
      }
    } else {
      # Fallback if the range doesn't match standard num/num
      r_in_range = paste0("# keep in range '", parsed$in_str, "' not fully translated.")
    }
  }

  # Build call to runtime execution function `scmd_keep`
  args = c("data = data")
  if (!is.na(parsed$varlist)) {
    args = c(args, paste0("varlist_str = ", quote_for_r_literal(parsed$varlist)))
  }
  if (!is.na(r_if_cond)) {
    args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  }
  if (!is.na(r_in_range)) {
    args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))
  }

  r_code_str = paste0("data = scmd_keep(", paste(args, collapse = ", "), ")")

  # Maintain package internal tracking variables
  if (isTRUE(stata2r_env$has_original_order_idx)) {
    r_code_str = paste0(r_code_str, " %>% \n  dplyr::mutate(stata2r_original_order_idx = dplyr::row_number())")
  }

  return(r_code_str)
}

# 3. Runtime Execution Phase: Evaluate against actual data columns and environments
scmd_keep = function(data, varlist_str = NA_character_, r_if_cond = NA_character_, r_in_range = NA_character_) {
  restore.point("scmd_keep")

  # 1. Row subsetting (`if` condition)
  if (!is.na(r_if_cond) && r_if_cond != "") {
    cond_expr = base::parse(text = r_if_cond)
    cond_val = base::eval(cond_expr, envir = data, enclos = parent.frame())
    # Stata treats NA in conditions as FALSE
    cond_val = dplyr::coalesce(as.logical(cond_val), FALSE)
    data = data[cond_val, , drop = FALSE]
  }

  # 2. Row subsetting (`in` range)
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = base::eval(base::parse(text = r_in_range))
    # Keep only indices that exist in the dataframe
    idx = idx[idx >= 1 & idx <= NROW(data)]
    data = data[idx, , drop = FALSE]
  }

  # 3. Column subsetting (`varlist`)
  if (!is.na(varlist_str) && varlist_str != "") {
    cols_to_keep = expand_varlist(varlist_str, names(data))
    if (length(cols_to_keep) == 0) {
      stop(paste0("scmd_keep: no variables found matching '", varlist_str, "'"))
    }

    # Always preserve the internal original order index if it exists in the incoming data
    if ("stata2r_original_order_idx" %in% names(data) && !("stata2r_original_order_idx" %in% cols_to_keep)) {
      cols_to_keep = c(cols_to_keep, "stata2r_original_order_idx")
    }

    data = data[, cols_to_keep, drop = FALSE]
  }

  return(data)
}
!END_CHANGE_FILE R/t_keep.R
***
