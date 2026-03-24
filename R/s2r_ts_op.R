# FILE: stata2r/R/s2r_ts_op.R

#' Resolve Time-Series Operators in Translated R Code
#' Processes a translation dataframe sequentially to track xtset/tsset state
#' and replaces TS operators with native collapse calls.
s2r_resolve_ts_operators = function(r_df, timevar = NULL, panelvar = NULL, tdelta = 1) {
  restore.point("s2r_resolve_ts_operators")
  # We assume r_df has columns like `cmdline` (original Stata) and `r_code` (translated R)
  cmd_col = intersect(c("do_code","stata_code","cmdlin"), names(r_df))[1]

  for (i in seq_len(nrow(r_df))) {
    stata_cmd = trimws(r_df[[cmd_col]][i])

    # 1. Update TS state if an xtset or tsset command is encountered
    if (grepl("^(xtset|tsset)\\b", stata_cmd, ignore.case = TRUE)) {
      hints = s2r_parse_xtset(stata_cmd, timevar, panelvar, tdelta)
      timevar  = hints$timevar
      panelvar = hints$panelvar
      tdelta   = hints$tdelta

      # Optionally emit R code to document the TS state change
      r_df$r_code[i] = paste0("# TS State Updated: panelvar=", panelvar,
                              ", timevar=", timevar, ", tdelta=", tdelta)
      next
    }

    # 2. Replace TS operators in the translated R code
    rc = r_df$r_code[i]
    if (!is.null(rc) && !is.na(rc) && rc != "") {
      rc = s2r_replace_ts_in_string(rc, timevar, panelvar, tdelta)
      r_df$r_code[i] = rc
    }
  }

  r_df
}

#' Parse xtset and tsset commands to extract panel and time variables
s2r_parse_xtset = function(cmd, timevar, panelvar, tdelta) {
  is_ts = grepl("^tsset\\b", cmd, ignore.case = TRUE)
  cmd_body = sub("^(xtset|tsset)\\s+", "", cmd, ignore.case = TRUE)

  # Extract delta(...) if present
  delta_match = stringi::stri_match_first_regex(cmd_body, "delta\\(([^)]+)\\)")
  if (!is.na(delta_match[1,1])) {
    tdelta = as.numeric(delta_match[1,2])
  }
  cmd_body = sub(",.*", "", cmd_body) # Strip options

  vars = strsplit(trimws(cmd_body), "\\s+")[[1]]
  vars = vars[vars != ""]

  if (length(vars) >= 2) {
    panelvar = vars[1]
    timevar  = vars[2]
  } else if (length(vars) == 1) {
    if (is_ts) {
      timevar  = vars[1]
      panelvar = NA_character_
    } else {
      # Stata allows 'xtset panelvar' without a time variable
      panelvar = vars[1]
      timevar  = NA_character_
    }
  }

  list(timevar = timevar, panelvar = panelvar, tdelta = tdelta)
}

#' Find TS operators (like d.x1) and replace with collapse calls
s2r_replace_ts_in_string = function(rc, timevar, panelvar, tdelta) {
  rx = "\\b([LlFfDdSsOo])([0-9]*)\\.([a-zA-Z0-9_]+)\\b"
  matches = stringi::stri_match_all_regex(rc, rx)[[1]]

  if (is.na(matches[1,1])) return(rc)

  for (i in seq_len(nrow(matches))) {
    full_match = matches[i, 1]
    op_type    = toupper(matches[i, 2])
    op_num     = matches[i, 3]
    op_num     = ifelse(op_num == "", 1, as.integer(op_num))
    base_var   = matches[i, 4]

    # Construct collapse call
    args = c(paste0("x = ", base_var))

    n_val = op_num
    diff_val = 1
    tdelta_val = if (!is.null(tdelta) && !is.na(tdelta)) as.numeric(tdelta) else 1

    if (op_type == "L") {
      fun = "sfun_flag"
      n_val = op_num
    } else if (op_type == "F") {
      fun = "sfun_flag"
      n_val = -op_num
    } else if (op_type == "D") {
      fun = "sfun_fdiff"
      diff_val = op_num
      n_val = 1
    } else if (op_type == "S") {
      fun = "sfun_fdiff"
      n_val = op_num
    } else if (op_type == "O") {
      # O. means omitted. Just fallback to the base variable
      # We strip backticks if they were added
      rc = gsub(paste0("`", full_match, "`"), base_var, rc, fixed = TRUE)
      rc = gsub(full_match, base_var, rc, fixed = TRUE)
      next
    }

    if (tdelta_val > 1 && op_type %in% c("L","F","D","S")) {
      n_val = n_val * tdelta_val
    }

    if (op_type %in% c("L", "F", "S")) {
      args = c(args, paste0("n = ", n_val))
    } else if (op_type == "D") {
      args = c(args, paste0("diff = ", diff_val))
      if (tdelta_val > 1) args = c(args, paste0("n = ", n_val))
    }

    if (!is.null(timevar) && !is.na(timevar) && timevar != "") {
      args = c(args, paste0("t = ", timevar))
    }
    if (!is.null(panelvar) && !is.na(panelvar) && panelvar != "") {
      args = c(args, paste0("g = ", panelvar))
    }

    r_expr = paste0(fun, "(", paste0(args, collapse = ", "), ")")

    # stata2r wraps variables with dots in backticks (e.g. `d.x1`).
    # We must replace the backticked version first, then the bare version.
    rc = gsub(paste0("`", full_match, "`"), r_expr, rc, fixed = TRUE)
    rc = gsub(full_match, r_expr, rc, fixed = TRUE)
  }

  rc
}
