# FILE: R/t_tabulate.R

# 1. Parsing Phase: Extract Stata syntax components
# 1. Parsing Phase: Extract Stata syntax components
s2r_p_tabulate = function(rest_of_cmd) {
  restore.point("s2r_p_tabulate")

  rest = stringi::stri_trim_both(rest_of_cmd)

  parts = stringi::stri_match_first_regex(
    rest,
    "^\\s*([^,]*?)(?:,\\s*(.*))?$"
  )

  main_part = stringi::stri_trim_both(parts[1, 2])
  options_str = stringi::stri_trim_both(parts[1, 3])

  parsed = s2r_parse_if_in(main_part)
  var_tokens = character(0)
  if (!is.na(parsed$base_str) && parsed$base_str != "") {
    var_tokens = stringi::stri_split_regex(parsed$base_str, "\\s+")[[1]]
    var_tokens = var_tokens[!is.na(var_tokens) & var_tokens != ""]
  }

  gen_stub = NA_character_
  if (!is.na(options_str) && options_str != "") {
    gen_match = stringi::stri_match_first_regex(
      options_str,
      "\\b(?:g|ge|gen|gene|gener|genera|generat|generate)\\s*\\(([^)]+)\\)"
    )
    if (!is.na(gen_match[1, 1])) {
      gen_stub = stringi::stri_trim_both(gen_match[1, 2])
    }
  }

  include_missing = FALSE
  if (!is.na(options_str) && options_str != "") {
    include_missing = fast_coalesce(
      stringi::stri_detect_regex(options_str, "(^|[[:space:],])missing($|[[:space:],])"),
      FALSE
    )
  }

  list(
    var_tokens = var_tokens,
    if_str = parsed$if_str,
    in_str = parsed$in_str,
    options = options_str,
    gen_stub = gen_stub,
    include_missing = include_missing
  )
}

# 2. Code Generation Phase: Emit R code
t_tabulate = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_tabulate")

  parsed = s2r_p_tabulate(rest_of_cmd)
  needed_r = unlist(cmd_obj$r_results_needed)

  has_gen = !is.na(parsed$gen_stub) && parsed$gen_stub != ""

  if (!has_gen && length(needed_r) == 0) {
    return(paste0("# tabulate at line ", line_num, " is no-op (no generated dummies or later-used r())."))
  }

  if (length(parsed$var_tokens) == 0) {
    return(paste0("# Failed to parse tabulate command: ", rest_of_cmd))
  }

  if (has_gen && length(parsed$var_tokens) != 1) {
    return(paste0("# tabulate generate() is only implemented for one-way tabulate: ", rest_of_cmd))
  }

  varname = parsed$var_tokens[1]

  r_if_cond = NA_character_
  if (!is.na(parsed$if_str) && parsed$if_str != "") {
    r_if_cond = translate_stata_expression_with_r_values(
      parsed$if_str,
      line_num,
      cmd_df,
      list(is_by_group = FALSE)
    )
  }

  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  args = c(
    "data = data",
    paste0("varname = ", quote_for_r_literal(varname)),
    paste0("needed_r = ", s2r_chr_vec_to_r(needed_r))
  )

  if (has_gen) {
    args = c(args, paste0("gen_stub = ", quote_for_r_literal(parsed$gen_stub)))
  }
  if (!is.na(r_if_cond)) {
    args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  }
  if (!is.na(r_in_range)) {
    args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))
  }

  args = c(args, paste0("include_missing = ", parsed$include_missing))

  r_code = paste0("s2r_tab_res = scmd_tabulate(", paste(args, collapse = ", "), ")")

  if (has_gen) {
    r_code = paste0(r_code, "\ndata = s2r_tab_res$data")
  }

  if (length(needed_r) > 0) {
    r_code = paste0(r_code, "\ns2r_store_r_results(s2r_tab_res$r_results)")
  }

  return(r_code)
}

# 3. Runtime Execution Phase: Evaluate against actual data
scmd_tabulate = function(data, varname, needed_r = character(0), gen_stub = NA_character_,
                         r_if_cond = NA_character_, r_in_range = NA_character_,
                         include_missing = FALSE) {
  restore.point("scmd_tabulate")

  var_actual = expand_varlist(varname, names(data))[1]
  if (is.na(var_actual) || !(var_actual %in% names(data))) {
    stop(paste0("scmd_tabulate: variable '", varname, "' not found"))
  }

  n = NROW(data)
  mask = rep(TRUE, n)

  if (!is.na(r_if_cond) && r_if_cond != "") {
    r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))
    mask = mask & s2r_eval_cond(data, r_if_cond, envir = parent.frame())
  }

  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask = rep(FALSE, n)
    in_mask[idx] = TRUE
    mask = mask & in_mask
  }

  x = data[[var_actual]]
  x_for_tab = x[mask]

  is_missing_value = function(v) {
    if (is.character(v)) {
      return(is.na(v) | stringi::stri_trim_both(v) == "")
    }
    is.na(v)
  }

  missing_x_for_tab = is_missing_value(x_for_tab)

  if (!include_missing) {
    levels_source = x_for_tab[!missing_x_for_tab]
  } else {
    levels_source = x_for_tab
  }

  if (is.character(levels_source) || is.factor(levels_source)) {
    levels_chr = as.character(levels_source)
    levels_chr[is.na(levels_chr)] = ""
    levs = unique(levels_chr)
    levs = stringi::stri_sort(levs, locale = "C")
  } else {
    levs = sort(unique(as.numeric(levels_source)), na.last = TRUE)
  }

  r_results = list()
  if ("r(N)" %in% needed_r) {
    if (include_missing) {
      r_results$r_N = sum(mask)
    } else {
      r_results$r_N = sum(mask & !is_missing_value(x))
    }
  }

  has_gen = !is.na(gen_stub) && gen_stub != ""
  if (has_gen) {
    if (length(levs) > 0) {
      new_names = paste0(gen_stub, seq_along(levs))
      existing_new = intersect(new_names, names(data))
      if (length(existing_new) > 0) {
        stop(paste0(
          "scmd_tabulate: generated variable(s) already exist: ",
          paste(existing_new, collapse = ", ")
        ))
      }

      x_cmp = x
      if (is.character(x_cmp) || is.factor(x_cmp)) {
        x_cmp = as.character(x_cmp)
        x_cmp[is.na(x_cmp)] = ""
      } else {
        x_cmp = as.numeric(x_cmp)
      }

      for (i in seq_along(levs)) {
        level_i = levs[i]
        new_var = new_names[i]

        if (is.character(x_cmp)) {
          match_i = x_cmp == as.character(level_i)
        } else if (is.na(level_i)) {
          match_i = is.na(x_cmp)
        } else {
          match_i = x_cmp == level_i
        }

        match_i[is.na(match_i)] = FALSE

        out = rep(NA_integer_, n)

        valid_mask = mask
        if (!include_missing) {
          valid_mask = mask & !is_missing_value(x)
        }

        out[valid_mask] = 0L
        out[valid_mask & match_i] = 1L

        data[[new_var]] = out
        attr(data[[new_var]], "label") = paste0(var_actual, "==", as.character(level_i))
      }
    }
  }

  return(list(data = data, r_results = r_results))
}
