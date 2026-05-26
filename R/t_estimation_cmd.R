# Shared estimation-side-effects translation and runtime helpers

t_estimation_cmd = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context, estimator = cmd_obj$stata_cmd) {
  restore.point("t_estimation_cmd")

  need_xi = isTRUE(cmd_obj$need_xi)

  if (!need_xi) {
    return(paste0("# ", estimator, " at line ", line_num, " is no-op (no later-used side effects)."))
  }

  parsed = s2r_p_estimation_cmd(rest_of_cmd, estimator = estimator)
  if (is.na(parsed$dep_var) && length(parsed$xi_specs) == 0) {
    return(paste0("# Failed to parse estimation command at line ", line_num, ": ", cmd_obj$do_code))
  }

  code_lines = character(0)

  if (length(parsed$xi_specs) == 0) {
    return(paste0("# Failed to parse xi side effects for estimation command at line ", line_num, ": ", cmd_obj$do_code))
  }

  for (spec in parsed$xi_specs) {
    if (is.null(spec$var2) || is.na(spec$var2) || spec$var2 == "") {
      code_lines = c(
        code_lines,
        paste0(
          "data = scmd_xi(data = data, var1 = ",
          quote_for_r_literal(spec$var1),
          ")"
        )
      )
    } else {
      code_lines = c(
        code_lines,
        paste0(
          "data = scmd_xi(data = data, var1 = ",
          quote_for_r_literal(spec$var1),
          ", var2 = ",
          quote_for_r_literal(spec$var2),
          ")"
        )
      )
    }
  }

  paste(code_lines, collapse = "\n")
}

scmd_estimation_effects = function(data, dep_var, model_vars, needed_e, r_if_cond = NA_character_, estimator = "regress", formula_terms = character(0)) {
  stop("scmd_estimation_effects is deprecated. R translations should load e() variables from repboxDRF caches.")
}

