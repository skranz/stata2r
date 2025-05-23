# Translate Stata 'generate' or 'gen' command

# Example Stata: gen newvar = oldvar * 2 if condition
# Example Stata: by group: gen seq = _n
t_generate = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  # Parse `rest_of_cmd` for new variable name, expression, and if condition
  # Example: "newvar = expression [if condition]"

  # Strip type if present (e.g. gen double newvar = ...)
  rest_of_cmd_no_type = stringi::stri_replace_first_regex(rest_of_cmd, "^(?:byte|int|long|float|double)\\s+", "")

  match = stringi::stri_match_first_regex(rest_of_cmd_no_type, "^\\s*([^=\\s]+)\\s*=\\s*(.*?)(?:\\s+if\\s+(.*))?$")

  if (is.na(match[1,1])) {
    return(paste0("# Failed to parse generate command: ", rest_of_cmd))
  }

  new_var = stringi::stri_trim_both(match[1,2])
  stata_expr = stringi::stri_trim_both(match[1,3])
  stata_if_cond = stringi::stri_trim_both(match[1,4]) # Might be NA

  # Context for expression translation (e.g. _n, _N behavior)
  # is_by_group TRUE if cmd_obj$by_group_vars is not NA
  current_context = list(is_by_group = cmd_obj$is_by_prefix && !is.na(cmd_obj$by_group_vars))
  r_expr = translate_stata_expression_with_r_values(stata_expr, line_num, cmd_df, current_context)

  r_if_cond = NA_character_
  if (!is.na(stata_if_cond) && stata_if_cond != "") {
    # The 'if' condition for generate/replace is evaluated row-wise on the whole dataset, not per group.
    r_if_cond = translate_stata_expression_with_r_values(stata_if_cond, line_num, cmd_df, context = list(is_by_group = FALSE))
  }

  # Prepare for by-processing steps as pipe components
  arrange_step = ""
  group_vars_r_vec_str = NULL
  
  if (cmd_obj$is_by_prefix) {
    if (!is.na(cmd_obj$by_group_vars)) {
      group_vars_list = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
      group_vars_list = group_vars_list[group_vars_list != ""]
      group_vars_r_vec_str = paste0('c("', paste0(group_vars_list, collapse='", "'), '")')
    }

    sort_vars_list = character(0)
    if (!is.na(cmd_obj$by_sort_vars)) {
      sort_vars_list = stringi::stri_split_fixed(cmd_obj$by_sort_vars, ",")[[1]]
      sort_vars_list = sort_vars_list[sort_vars_list != ""]
    }

    # If there are sort keys for by-processing (e.g., for lags/leads), arrange data first
    if (length(sort_vars_list) > 0) {
      all_sort_vars = c(if(!is.null(group_vars_list)) group_vars_list else character(0), sort_vars_list)
      all_sort_vars_str = paste(all_sort_vars, collapse = ", ")
      arrange_step = paste0("dplyr::arrange(data, ", all_sort_vars_str, ")")
    }
  }

  if (!is.na(r_if_cond) && r_if_cond != "") {
    mutate_expr = paste0(new_var, " = dplyr::if_else(", r_if_cond, ", ", r_expr, ", NA_real_)")
  } else {
    mutate_expr = paste0(new_var, " = ", r_expr)
  }

  # Build the R code string using pipes
  r_code_parts = c()

  if (arrange_step != "") {
      r_code_parts = c(r_code_parts, arrange_step)
  } else {
      r_code_parts = c(r_code_parts, "data") # Start with data if no arrange step
  }

  if (!is.null(group_vars_r_vec_str)) {
      r_code_parts = c(r_code_parts, paste0("  %>% collapse::fgroup_by(", group_vars_r_vec_str, ")"))
      r_code_parts = c(r_code_parts, paste0("  %>% collapse::fmutate(", mutate_expr, ")"))
      r_code_parts = c(r_code_parts, paste0("  %>% collapse::fungroup()"))
  } else {
      # If not grouped, still use a pipe for fmutate for consistency and clarity
      if (arrange_step == "") { # If no arrange, first pipe is directly to fmutate
          r_code_parts = c(r_code_parts, paste0("  %>% collapse::fmutate(", mutate_expr, ")"))
      } else { # If arrange exists, fmutate is another pipe in the chain
          r_code_parts = c(r_code_parts, paste0("  %>% collapse::fmutate(", mutate_expr, ")"))
      }
  }

  # Final assignment: data = first_part %>% ...
  r_code_str = paste0("data = ", paste(r_code_parts, collapse="\n"))
  
  return(r_code_str)
}

