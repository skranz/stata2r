To fix the issue where `replace varname="Age_C" in 2` causes an unexpected symbol error, we need to correct the parsing of `in` ranges for commands like `generate` and `replace`. We also need to correctly combine the `in` range and `if` condition using a pre-computed Boolean mask column (`.stata_temp_mask`) because a raw translated range index string (like `2:2`) cannot be injected directly into a `dplyr::if_else` condition.

Here are the fixes for `t_replace.R`, `t_generate.R`, `t_recode.R`, and `t_egen.R`.

!MODIFICATION s2r_p_replace t_replace.R
scope = "function"
file = "t_replace.R"
function_name = "s2r_p_replace"
description = "Update `s2r_p_replace` to handle if/in parsing using s2r_parse_if_in."
---
```r
# 1. Parsing Phase: Extract Stata syntax components
s2r_p_replace = function(rest_of_cmd) {
  restore.point("s2r_p_replace")
  explicit_type_match = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*(?:byte|int|long|float|double|str\\d+|strL)\\s+")
  declared_type_str = if (!is.na(explicit_type_match[1,1])) explicit_type_match[1,2] else NA_character_

  rest_no_type = stringi::stri_replace_first_regex(rest_of_cmd, "^\\s*(?:byte|int|long|float|double|str\\d+|strL)\\s+", "")
  
  parts = stringi::stri_split_fixed(rest_no_type, "=", n=2)[[1]]
  if (length(parts) < 2) {
    return(list(
      declared_type = declared_type_str,
      var_to_replace = NA_character_,
      stata_expr = NA_character_,
      if_cond = NA_character_,
      in_str = NA_character_
    ))
  }

  var_to_replace = stringi::stri_trim_both(parts[1])
  right_side = stringi::stri_trim_both(parts[2])
  
  parsed = s2r_parse_if_in(right_side)

  list(
    declared_type = declared_type_str,
    var_to_replace = var_to_replace,
    stata_expr = parsed$base_str,
    if_cond = parsed$if_str,
    in_str = parsed$in_str
  )
}
```
!END_MODIFICATION s2r_p_replace t_replace.R

!MODIFICATION t_replace t_replace.R
scope = "function"
file = "t_replace.R"
function_name = "t_replace"
description = "Update `t_replace` to pass r_in_range."
---
```r
# 2. Code Generation Phase: Emit R code
t_replace = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_replace")
  parsed = s2r_p_replace(rest_of_cmd)
  if (is.na(parsed$var_to_replace)) return(paste0("# Failed to parse replace command: ", rest_of_cmd))

  current_context = list(is_by_group = cmd_obj$is_by_prefix && length(cmd_obj$by_group_vars) > 0 && !is.na(cmd_obj$by_group_vars[1]))
  r_expr = translate_stata_expression_with_r_values(parsed$stata_expr, line_num, cmd_df, current_context)
  if (is.na(r_expr)) r_expr = "NA_real_"

  r_if_cond = NA_character_
  if (!is.na(parsed$if_cond) && parsed$if_cond != "") {
    r_if_cond = translate_stata_expression_with_r_values(parsed$if_cond, line_num, cmd_df, list(is_by_group = FALSE))
  }
  
  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  group_vars_list_bare = character(0)
  if (current_context$is_by_group) {
    group_vars_list = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
    group_vars_list_bare = group_vars_list[!is.na(group_vars_list) & group_vars_list != ""]
  }

  is_string = sfun_is_stata_expression_string_typed(parsed$stata_expr)
  force_integer = FALSE
  if (!is.na(parsed$declared_type)) {
    is_string = stringi::stri_startswith_fixed(parsed$declared_type, "str")
    force_integer = parsed$declared_type %in% c("byte", "int", "long")
  }

  args = c("data = data", paste0("var_to_replace = ", quote_for_r_literal(parsed$var_to_replace)), paste0("r_expr_str = ", quote_for_r_literal(r_expr)))
  if (!is.na(r_if_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  if (!is.na(r_in_range)) args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))

  if (length(group_vars_list_bare) > 0) args = c(args, paste0("group_vars = c('", paste(group_vars_list_bare, collapse="','"), "')"))
  args = c(args, paste0("is_string = ", is_string), paste0("force_integer = ", force_integer))

  return(paste0("data = scmd_replace(", paste(args, collapse = ", "), ")"))
}
```
!END_MODIFICATION t_replace t_replace.R

!MODIFICATION scmd_replace t_replace.R
scope = "function"
file = "t_replace.R"
function_name = "scmd_replace"
description = "Update `scmd_replace` to handle if/in properly with mask and avoid using data$var in mutate."
---
```r
# 3. Runtime Execution Phase: Evaluate against actual data
scmd_replace = function(data, var_to_replace, r_expr_str, r_if_cond = NA_character_, r_in_range = NA_character_, group_vars = character(0), is_string = FALSE, force_integer = FALSE) {
  restore.point("scmd_replace")

  var_actual = expand_varlist(var_to_replace, names(data))[1]
  r_expr_str = resolve_abbrevs_in_expr(r_expr_str, names(data))
  r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))

  # If the target is character, we safely flag it as string to prevent NA coercion
  target_is_char = is.character(data[[var_actual]])
  is_string = is_string || target_is_char

  mask_expr = ".stata_temp_mask"
  if (!is.na(r_if_cond) && r_if_cond != "") {
    mask_expr = paste0("(.stata_temp_mask & fast_coalesce(as.numeric(", r_if_cond, "), 0) != 0)")
  }
  
  if (r_expr_str == "NA_real_") {
    if (is_string) {
       expr_body = paste0("dplyr::if_else(", mask_expr, ", '', `", var_actual, "`)")
    } else {
       expr_body = paste0("dplyr::if_else(", mask_expr, ", NA_real_, `", var_actual, "`)")
    }
  } else {
    if (is_string) {
        expr_body = paste0("{ .val <- as.character(", r_expr_str, "); dplyr::if_else(", mask_expr, ", .val, `", var_actual, "`) }")
    } else if (force_integer) {
        expr_body = paste0("{ .val <- as.integer(", r_expr_str, "); dplyr::if_else(", mask_expr, ", .val, `", var_actual, "`) }")
    } else {
        expr_body = paste0("{ .val <- ", r_expr_str, "; if(is.character(.val)) { dplyr::if_else(", mask_expr, ", .val, `", var_actual, "`) } else { .val <- as.numeric(.val); dplyr::if_else(", mask_expr, ", .val, `", var_actual, "`) } }")
    }
  }

  # Compute in-range mask globally
  in_mask = rep(TRUE, nrow(data))
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask_vec = rep(FALSE, nrow(data))
    in_mask_vec[idx] = TRUE
    in_mask = in_mask_vec
  }
  data$.stata_temp_mask = in_mask

  pipe_el = c("data")
  group_vars = expand_varlist(paste(group_vars, collapse=" "), names(data))
  if (length(group_vars) > 0) pipe_el = c(pipe_el, paste0("dplyr::group_by(!!!dplyr::syms(c('", paste(group_vars, collapse="','"), "')))"))
  pipe_el = c(pipe_el, paste0("dplyr::mutate(`", var_actual, "` = ", expr_body, ")"))
  if (length(group_vars) > 0) pipe_el = c(pipe_el, "dplyr::ungroup()")

  data = eval(parse(text = paste(pipe_el, collapse = " %>% ")), envir = list(data = data), enclos = parent.frame())
  data$.stata_temp_mask = NULL
  
  return(data)
}
```
!END_MODIFICATION scmd_replace t_replace.R

!MODIFICATION s2r_p_generate t_generate.R
scope = "function"
file = "t_generate.R"
function_name = "s2r_p_generate"
description = "Update `s2r_p_generate` to parse if/in using general package utility."
---
```r
# 1. Parsing Phase: Extract Stata syntax components
s2r_p_generate = function(rest_of_cmd) {
  restore.point("s2r_p_generate")
  explicit_type_match = stringi::stri_match_first_regex(rest_of_cmd, "^\\s*(?:byte|int|long|float|double|str\\d+|strL)\\s+")
  declared_type_str = if (!is.na(explicit_type_match[1,1])) explicit_type_match[1,2] else NA_character_

  rest_no_type = stringi::stri_replace_first_regex(rest_of_cmd, "^\\s*(?:byte|int|long|float|double|str\\d+|strL)\\s+", "")
  
  parts = stringi::stri_split_fixed(rest_no_type, "=", n=2)[[1]]
  if (length(parts) < 2) {
    return(list(
      declared_type = declared_type_str,
      new_var = NA_character_,
      stata_expr = NA_character_,
      if_cond = NA_character_,
      in_str = NA_character_
    ))
  }
  
  new_var = stringi::stri_trim_both(parts[1])
  right_side = stringi::stri_trim_both(parts[2])
  
  parsed = s2r_parse_if_in(right_side)
  
  list(
    declared_type = declared_type_str,
    new_var = new_var,
    stata_expr = parsed$base_str,
    if_cond = parsed$if_str,
    in_str = parsed$in_str
  )
}
```
!END_MODIFICATION s2r_p_generate t_generate.R

!MODIFICATION t_generate t_generate.R
scope = "function"
file = "t_generate.R"
function_name = "t_generate"
description = "Update `t_generate` to supply r_in_range to scmd_generate."
---
```r
# 2. Code Generation Phase: Emit R code
t_generate = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_generate")
  parsed = s2r_p_generate(rest_of_cmd)
  if (is.na(parsed$new_var)) return(paste0("# Failed to parse generate command: ", rest_of_cmd))

  current_context = list(is_by_group = cmd_obj$is_by_prefix && length(cmd_obj$by_group_vars) > 0 && !is.na(cmd_obj$by_group_vars[1]))
  r_expr = translate_stata_expression_with_r_values(parsed$stata_expr, line_num, cmd_df, current_context)
  if (is.na(r_expr)) r_expr = "NA_real_"

  r_if_cond = NA_character_
  if (!is.na(parsed$if_cond) && parsed$if_cond != "") {
    r_if_cond = translate_stata_expression_with_r_values(parsed$if_cond, line_num, cmd_df, list(is_by_group = FALSE))
  }
  
  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  group_vars_list_bare = character(0)
  if (current_context$is_by_group) {
    group_vars_list = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
    group_vars_list_bare = group_vars_list[!is.na(group_vars_list) & group_vars_list != ""]
  }

  is_string = sfun_is_stata_expression_string_typed(parsed$stata_expr)
  force_integer = FALSE
  if (!is.na(parsed$declared_type)) {
    is_string = stringi::stri_startswith_fixed(parsed$declared_type, "str")
    force_integer = parsed$declared_type %in% c("byte", "int", "long")
  }

  args = c("data = data", paste0("new_var = ", quote_for_r_literal(parsed$new_var)), paste0("r_expr_str = ", quote_for_r_literal(r_expr)))
  if (!is.na(r_if_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  if (!is.na(r_in_range)) args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))

  if (length(group_vars_list_bare) > 0) args = c(args, paste0("group_vars = c('", paste(group_vars_list_bare, collapse="','"), "')"))
  args = c(args, paste0("is_string = ", is_string), paste0("force_integer = ", force_integer))

  return(paste0("data = scmd_generate(", paste(args, collapse = ", "), ")"))
}
```
!END_MODIFICATION t_generate t_generate.R

!MODIFICATION scmd_generate t_generate.R
scope = "function"
file = "t_generate.R"
function_name = "scmd_generate"
description = "Update `scmd_generate` to handle mask expression combining if and in clauses seamlessly via .stata_temp_mask."
---
```r
# 3. Runtime Execution Phase: Evaluate against actual data
scmd_generate = function(data, new_var, r_expr_str, r_if_cond = NA_character_, r_in_range = NA_character_, group_vars = character(0), is_string = FALSE, force_integer = FALSE) {
  restore.point("scmd_generate")

  r_expr_str = resolve_abbrevs_in_expr(r_expr_str, names(data))
  r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))

  mask_expr = ".stata_temp_mask"
  if (!is.na(r_if_cond) && r_if_cond != "") {
    mask_expr = paste0("(.stata_temp_mask & fast_coalesce(as.numeric(", r_if_cond, "), 0) != 0)")
  }
  
  if (r_expr_str == "NA_real_") {
    # special case if r_expr_str is exactly NA_real_ and is_string is TRUE
    if (is_string) {
       expr_body = paste0("dplyr::if_else(", mask_expr, ", '', '')")
    } else {
       expr_body = paste0("dplyr::if_else(", mask_expr, ", NA_real_, NA_real_)")
    }
  } else {
    if (is_string) {
        expr_body = paste0("{ .val <- as.character(", r_expr_str, "); dplyr::if_else(", mask_expr, ", .val, '') }")
    } else if (force_integer) {
        expr_body = paste0("{ .val <- as.integer(", r_expr_str, "); dplyr::if_else(", mask_expr, ", .val, NA_integer_) }")
    } else {
        expr_body = paste0("{ .val <- ", r_expr_str, "; if(is.character(.val)) { dplyr::if_else(", mask_expr, ", .val, '') } else { .val <- as.numeric(.val); dplyr::if_else(", mask_expr, ", .val, NA_real_) } }")
    }
  }

  # Compute in-range mask globally
  in_mask = rep(TRUE, nrow(data))
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask_vec = rep(FALSE, nrow(data))
    in_mask_vec[idx] = TRUE
    in_mask = in_mask_vec
  }
  data$.stata_temp_mask = in_mask

  pipe_el = c("data")
  group_vars = expand_varlist(paste(group_vars, collapse=" "), names(data))
  if (length(group_vars) > 0) pipe_el = c(pipe_el, paste0("dplyr::group_by(!!!dplyr::syms(c('", paste(group_vars, collapse="','"), "')))"))
  pipe_el = c(pipe_el, paste0("dplyr::mutate(`", new_var, "` = ", expr_body, ")"))
  if (length(group_vars) > 0) pipe_el = c(pipe_el, "dplyr::ungroup()")

  data = eval(parse(text = paste(pipe_el, collapse = " %>% ")), envir = list(data = data), enclos = parent.frame())
  data$.stata_temp_mask = NULL
  return(data)
}
```
!END_MODIFICATION scmd_generate t_generate.R

!MODIFICATION t_recode t_recode.R
scope = "function"
file = "t_recode.R"
function_name = "t_recode"
description = "Update `t_recode` to pass `r_in_range`."
---
```r
# 2. Code Generation Phase: Emit R code
t_recode = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_recode")
  parsed = s2r_p_recode(rest_of_cmd)
  if (is.na(parsed$varlist) || length(parsed$rules) == 0) return(paste0("# Failed to parse recode command: ", rest_of_cmd))

  any_rule_implies_string_output = FALSE
  any_rule_implies_labelled_numeric_output = FALSE
  collected_labels = list()

  for (rule_raw in parsed$rules) {
    parts_eq = stringi::stri_split_fixed(stringi::stri_trim_both(rule_raw), "=", n=2)[[1]]
    if (length(parts_eq) != 2) next
    new_part_raw = stringi::stri_trim_both(parts_eq[2])

    if ((fast_coalesce(stringi::stri_startswith_fixed(new_part_raw, '"'), FALSE)) ||
        (fast_coalesce(stringi::stri_startswith_fixed(new_part_raw, "'"), FALSE))) {
      any_rule_implies_string_output = TRUE
    }

    label_match = stringi::stri_match_first_regex(new_part_raw, "^\\s*([^\\s]+)\\s+(?:\"([^\"]*)\"|'([^']*)')\\s*$")
    if (!is.na(label_match[1,1])) {
      any_rule_implies_labelled_numeric_output = TRUE
      numeric_val_part = stringi::stri_trim_both(label_match[1,2])
      string_label_part = ifelse(!is.na(label_match[1,3]), label_match[1,3], label_match[1,4])

      r_numeric_val = NA_real_
      if (numeric_val_part != "." && !stringi::stri_detect_regex(numeric_val_part, "^\\.[a-zA-Z]$")) {
        r_numeric_val = as.numeric(numeric_val_part)
      }
      if (!is.na(r_numeric_val) && !is.na(string_label_part)) {
        collected_labels[[length(collected_labels) + 1]] = list(label = string_label_part, value = r_numeric_val)
      }
    }
  }

  final_r_var_type_is_string = any_rule_implies_string_output
  final_r_var_type_is_labelled_numeric = !final_r_var_type_is_string && any_rule_implies_labelled_numeric_output

  final_labels_map = list()
  if (final_r_var_type_is_labelled_numeric && length(collected_labels) > 0) {
    temp_df_labels = data.frame(
        label = sapply(collected_labels, `[[`, "label"),
        value = sapply(collected_labels, `[[`, "value"),
        stringsAsFactors = FALSE
    )
    temp_df_labels$original_order = seq_len(NROW(temp_df_labels))
    temp_df_labels = temp_df_labels[order(temp_df_labels$value, -temp_df_labels$original_order), ]
    temp_df_labels = temp_df_labels[!duplicated(temp_df_labels$value, fromLast = TRUE), ]
    temp_df_labels = temp_df_labels[order(temp_df_labels$value), ]

    final_labels_map = stats::setNames(temp_df_labels$value, temp_df_labels$label)
  }

  r_subset_cond = NA_character_
  if (!is.na(parsed$if_str)) r_subset_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, list(is_by_group = FALSE))

  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  r_rules_templates = sapply(parsed$rules, translate_recode_rule_template, final_r_var_type_is_string = final_r_var_type_is_string)
  quoted_rules = sapply(r_rules_templates, quote_for_r_literal)

  args = c("data = data", paste0("varlist_str = ", quote_for_r_literal(parsed$varlist)),
           paste0("rules_templates = c(", paste(quoted_rules, collapse=", "), ")"))
  if (!is.na(parsed$gen_vars)) args = c(args, paste0("gen_vars_str = ", quote_for_r_literal(parsed$gen_vars)))
  if (!is.na(r_subset_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_subset_cond)))
  if (!is.na(r_in_range)) args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))
  args = c(args, paste0("is_string = ", final_r_var_type_is_string))

  if (final_r_var_type_is_labelled_numeric && length(final_labels_map) > 0) {
    labels_vector_r_code = paste0("stats::setNames(c(", paste0(unname(final_labels_map), collapse=", "), "), c(", paste0('"', names(final_labels_map), '"', collapse=", "), "))")
    args = c(args, paste0("labels_map = ", labels_vector_r_code))
  }

  return(paste0("data = scmd_recode(", paste(args, collapse = ", "), ")"))
}
```
!END_MODIFICATION t_recode t_recode.R

!MODIFICATION scmd_recode t_recode.R
scope = "function"
file = "t_recode.R"
function_name = "scmd_recode"
description = "Update `scmd_recode` to utilize global `.stata_temp_mask` mechanism similar to replace/generate."
---
```r
scmd_recode = function(data, varlist_str, rules_templates, gen_vars_str = NA_character_, r_if_cond = NA_character_, r_in_range = NA_character_, is_string = FALSE, labels_map = NULL) {
  restore.point("scmd_recode")

  vars_actual = expand_varlist(varlist_str, names(data))

  new_vars = vars_actual
  if (!is.na(gen_vars_str)) {
    new_vars = stringi::stri_split_regex(gen_vars_str, "\\s+")[[1]]
    new_vars = new_vars[new_vars != ""]
    if (length(new_vars) != length(vars_actual)) stop("scmd_recode: gen() requires same number of vars.")
  }

  r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))

  mask_expr = ".stata_temp_mask"
  if (!is.na(r_if_cond) && r_if_cond != "") {
    mask_expr = paste0("(.stata_temp_mask & fast_coalesce(as.numeric(", r_if_cond, "), 0) != 0)")
  }

  # Compute in-range mask globally
  in_mask = rep(TRUE, nrow(data))
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask_vec = rep(FALSE, nrow(data))
    in_mask_vec[idx] = TRUE
    in_mask = in_mask_vec
  }
  data$.stata_temp_mask = in_mask

  for (i in seq_along(vars_actual)) {
    old_var = vars_actual[i]
    new_var = new_vars[i]

    # Dynamically capture character targets to prevent NA string coercion
    target_is_char = is.character(data[[old_var]])
    current_is_string = is_string || target_is_char

    old_attrs = attributes(data[[old_var]])

    r_rules = gsub(".VAR.", paste0("`", old_var, "`"), rules_templates, fixed = TRUE)
    # Also resolve abbreviations in rules
    r_rules = vapply(r_rules, function(r) resolve_abbrevs_in_expr(r, names(data)), character(1))

    # STATA FALLBACK: If values do not match any conditions, they are left unchanged.
    fallback = if (current_is_string) paste0("as.character(`", old_var, "`)") else paste0("as.numeric(`", old_var, "`)")
    r_rules = c(r_rules, paste0("TRUE ~ ", fallback))

    case_when_expr = paste0("dplyr::case_when(\n    ", paste(r_rules, collapse = ",\n    "), "\n  )")

    if (mask_expr != ".stata_temp_mask" || (!is.na(r_in_range) && r_in_range != "")) {
      final_val_expr = paste0("dplyr::if_else(", mask_expr, ", ", case_when_expr, ", `", old_var, "`)")
    } else {
      final_val_expr = case_when_expr
    }

    if (current_is_string) {
      final_val_expr = paste0("as.character(", final_val_expr, ")")
    } else {
      final_val_expr = paste0("as.numeric(", final_val_expr, ")")
    }

    data = eval(parse(text = paste0("dplyr::mutate(data, `", new_var, "` = ", final_val_expr, ")")), envir = list(data = data), enclos = parent.frame())

    # Restore or Assign Labels
    if (!is.null(labels_map)) {
      data[[new_var]] = haven::labelled(data[[new_var]], labels = labels_map)
    } else if (old_var == new_var && !current_is_string) {
      # If replacing and no new labels are provided, keep original labels matching Stata
      if (!is.null(old_attrs$labels)) attr(data[[new_var]], "labels") = old_attrs$labels
      if (!is.null(old_attrs$label)) attr(data[[new_var]], "label") = old_attrs$label
      if (!is.null(old_attrs$class) && "haven_labelled" %in% old_attrs$class) {
        class(data[[new_var]]) = old_attrs$class
      }
    }
  }

  data$.stata_temp_mask = NULL
  return(data)
}
```
!END_MODIFICATION scmd_recode t_recode.R

!MODIFICATION t_egen t_egen.R
scope = "function"
file = "t_egen.R"
function_name = "t_egen"
description = "Update `t_egen` to properly handle `r_in_range` by passing it separately to scmd_egen."
---
```r
# 2. Code Generation Phase
t_egen = function(rest_of_cmd, cmd_obj, cmd_df, line_num, context) {
  restore.point("t_egen")
  parsed = s2r_p_egen(rest_of_cmd)
  if (is.na(parsed$new_var)) return(paste0("# Failed to parse egen command: ", rest_of_cmd))

  r_if_cond = NA_character_
  if (!is.na(parsed$if_str)) r_if_cond = translate_stata_expression_with_r_values(parsed$if_str, line_num, cmd_df, context)
  r_in_range = s2r_in_str_to_r_range_str(parsed$in_str)

  # For egen, we cannot simply combine r_in_range directly with r_if_cond as text because r_in_range is an index vector (e.g. "1:5")
  # We will pass r_in_range as a separate argument to scmd_egen.
  
  r_args = translate_stata_expression_with_r_values(parsed$args_str, line_num, cmd_df, context)
  
  # Inside scmd_egen we will have a .stata_temp_mask that incorporates if and in conditions.
  r_args_cond = paste0("dplyr::if_else(.stata_temp_mask, ", r_args, ", NA)")

  is_ftm = fast_coalesce(stringi::stri_detect_fixed(parsed$options, "fieldstrustmissings"), FALSE)
  is_row = FALSE
  needs_temp_sort = FALSE

  calc_expr = ""
  if (parsed$func_name == "mean") calc_expr = paste0("mean(", r_args_cond, ", na.rm = TRUE)")
  else if (parsed$func_name %in% c("total", "sum")) calc_expr = paste0("collapse::fsum(", r_args_cond, ", na.rm = TRUE)")
  else if (parsed$func_name == "count") calc_expr = paste0("sum(!is.na(", r_args_cond, "))")
  else if (parsed$func_name == "min") calc_expr = paste0("collapse::fmin(", r_args_cond, ", na.rm = TRUE)")
  else if (parsed$func_name == "max") calc_expr = paste0("collapse::fmax(", r_args_cond, ", na.rm = TRUE)")
  else if (parsed$func_name == "rank") {
    needs_temp_sort = !cmd_obj$is_by_prefix
    if (is_ftm) val = paste0("as.numeric(dplyr::if_else(is.na(", r_args_cond, "), Inf, ", r_args_cond, "))") else val = r_args_cond
    calc_expr = paste0("as.numeric(base::rank(", val, ", ties.method = 'average', na.last = 'keep'))")
  }
  else if (parsed$func_name %in% c("median", "p50")) calc_expr = paste0("stats::median(", r_args_cond, ", na.rm = TRUE)")
  else if (parsed$func_name %in% c("sd", "std")) calc_expr = paste0("stats::sd(", r_args_cond, ", na.rm = TRUE)")
  else if (parsed$func_name == "group") { needs_temp_sort = !cmd_obj$is_by_prefix; calc_expr = "dplyr::cur_group_id()" }
  else if (parsed$func_name == "tag") { needs_temp_sort = !cmd_obj$is_by_prefix; calc_expr = "as.numeric(dplyr::row_number() == 1)" }
  else if (parsed$func_name %in% c("rowtotal", "rowmean", "concat")) {
    is_row = TRUE
    calc_expr = paste0(".ROWOP_", parsed$func_name, "_PLACEHOLDER.")
  }
  else if (parsed$func_name == "seq") {
    needs_temp_sort = !cmd_obj$is_by_prefix
    from_val = 1
    to_val = NA_real_
    block_val = 1
    if (!is.na(parsed$options)) {
      from_match = stringi::stri_match_first_regex(parsed$options, "\\bfrom\\s*\\((\\d+)\\)")
      if (!is.na(from_match[1,1])) from_val = as.numeric(from_match[1,2])
      to_match = stringi::stri_match_first_regex(parsed$options, "\\bto\\s*\\((\\d+)\\)")
      if (!is.na(to_match[1,1])) to_val = as.numeric(to_match[1,2])
      block_match = stringi::stri_match_first_regex(parsed$options, "\\bblock\\s*\\((\\d+)\\)")
      if (!is.na(block_match[1,1])) block_val = as.numeric(block_match[1,2])
    }
    if (block_val == 1) {
      if (from_val == 1) {
        calc_expr = "dplyr::row_number()"
      } else {
        calc_expr = paste0("dplyr::row_number() + ", from_val - 1)
      }
    } else {
      calc_expr = paste0("floor((dplyr::row_number() - 1) / ", block_val, ") + ", from_val)
    }
    if (!is.na(to_val)) {
      calc_expr = paste0("floor((dplyr::row_number() - 1) / ", block_val, ") %% (", to_val - from_val + 1, ") + ", from_val)
    }
    calc_expr = paste0("dplyr::if_else(.stata_temp_mask, as.numeric(", calc_expr, "), NA_real_)")
  }
  else return(paste0("# Egen func '", parsed$func_name, "' not implemented."))

  group_vars = character(0)
  if (cmd_obj$is_by_prefix) group_vars = stringi::stri_split_fixed(cmd_obj$by_group_vars, ",")[[1]]
  else if (!is.na(parsed$options)) {
    by_opt = stringi::stri_match_first_regex(parsed$options, "\\bby\\s*\\(([^)]+)\\)")
    if (!is.na(by_opt[1,1])) group_vars = stringi::stri_split_regex(stringi::stri_trim_both(by_opt[1,2]), "\\s+")[[1]]
  }
  group_vars = group_vars[group_vars != "" & !is.na(group_vars)]

  args = c("data = data", paste0("new_var = ", quote_for_r_literal(parsed$new_var)),
           paste0("func_name = ", quote_for_r_literal(parsed$func_name)),
           paste0("calc_expr = ", quote_for_r_literal(calc_expr)))

  if (!is.na(r_if_cond)) args = c(args, paste0("r_if_cond = ", quote_for_r_literal(r_if_cond)))
  if (!is.na(r_in_range)) args = c(args, paste0("r_in_range = ", quote_for_r_literal(r_in_range)))

  if (length(group_vars) > 0) args = c(args, paste0("group_vars = c('", paste(group_vars, collapse="','"), "')"))
  if (parsed$func_name %in% c("group", "tag", "rowtotal", "rowmean", "concat")) args = c(args, paste0("args_str = ", quote_for_r_literal(parsed$args_str)))
  args = c(args, paste0("needs_temp_sort = ", needs_temp_sort), paste0("is_row = ", is_row))

  return(paste0("data = scmd_egen(", paste(args, collapse = ", "), ")"))
}
```
!END_MODIFICATION t_egen t_egen.R

!MODIFICATION scmd_egen t_egen.R
scope = "function"
file = "t_egen.R"
function_name = "scmd_egen"
description = "Update `scmd_egen` to evaluate mask and r_in_range accurately before computing calculations."
---
```r
scmd_egen = function(data, new_var, func_name, calc_expr, r_if_cond = NA_character_, r_in_range = NA_character_, group_vars = character(0), args_str = NA_character_, needs_temp_sort = FALSE, is_row = FALSE) {
  restore.point("scmd_egen")

  group_vars_actual = expand_varlist(paste(group_vars, collapse=" "), names(data))
  if (func_name %in% c("group", "tag")) {
    arg_vars = expand_varlist(args_str, names(data))
    group_vars_actual = unique(c(group_vars_actual, arg_vars))
  }

  r_if_cond = resolve_abbrevs_in_expr(r_if_cond, names(data))

  # Compute mask globally
  in_mask = rep(TRUE, nrow(data))
  if (!is.na(r_in_range) && r_in_range != "") {
    idx = s2r_eval_range(data, r_in_range)
    in_mask_vec = rep(FALSE, nrow(data))
    in_mask_vec[idx] = TRUE
    in_mask = in_mask_vec
  }
  
  if (!is.na(r_if_cond) && r_if_cond != "") {
    data$.stata_temp_mask = in_mask & (fast_coalesce(as.numeric(s2r_eval_cond(data, r_if_cond, envir = parent.frame())), 0) != 0)
  } else {
    data$.stata_temp_mask = in_mask
  }

  if (is_row) {
    row_vars = expand_varlist(args_str, names(data))
    if (func_name == "rowtotal") {
      calc_expr = paste0("dplyr::if_else(.stata_temp_mask, base::rowSums(replace(dplyr::select(data, dplyr::all_of(c('", paste(row_vars, collapse="','"), "'))), is.na(dplyr::select(data, dplyr::all_of(c('", paste(row_vars, collapse="','"), "')))), 0), na.rm = FALSE), NA_real_)")
    } else if (func_name == "rowmean") {
      calc_expr = paste0("dplyr::if_else(.stata_temp_mask, base::rowMeans(dplyr::select(data, dplyr::all_of(c('", paste(row_vars, collapse="','"), "'))), na.rm = TRUE), NA_real_)")
    } else if (func_name == "concat") {
      na_checks = paste0("is.na(data[['", row_vars, "']])", collapse=" & ")
      stri_args = paste0("dplyr::if_else(is.na(as.character(data[['", row_vars, "']])), \"\", as.character(data[['", row_vars, "']]))", collapse=", ")
      calc_expr = paste0("dplyr::if_else(!.stata_temp_mask | (", na_checks, "), NA_character_, stringi::stri_paste(", stri_args, ", sep = ''))")
    }
  } else {
    calc_expr = resolve_abbrevs_in_expr(calc_expr, names(data))
  }

  if (needs_temp_sort) {
    tmp = data
    sort_vars = group_vars_actual
    if (func_name == "rank") sort_vars = unique(c(sort_vars, expand_varlist(args_str, names(data))))
    if ("stata2r_original_order_idx" %in% names(tmp)) sort_vars = c(sort_vars, "stata2r_original_order_idx")

    if (length(sort_vars) > 0) {
      sort_cmd = paste0("dplyr::arrange(tmp, ", paste(paste0("`", sort_vars, "`"), collapse=", "), ")")
      tmp = eval(parse(text = sort_cmd), envir = list(tmp = tmp), enclos = parent.frame())
    }

    pipe_el = c("tmp")
    if (length(group_vars_actual) > 0) pipe_el = c(pipe_el, paste0("dplyr::group_by(!!!dplyr::syms(c('", paste(group_vars_actual, collapse="','"), "')))"))
    pipe_el = c(pipe_el, paste0("dplyr::mutate(`", new_var, "` = ", calc_expr, ")"))
    if (length(group_vars_actual) > 0) pipe_el = c(pipe_el, "dplyr::ungroup()")

    tmp = eval(parse(text = paste(pipe_el, collapse = " %>% ")), envir = list(tmp = tmp), enclos = parent.frame())

    if ("stata2r_original_order_idx" %in% names(data)) {
      data = dplyr::left_join(data, tmp[, c("stata2r_original_order_idx", new_var)], by = "stata2r_original_order_idx")
    } else {
      data[[new_var]] = tmp[[new_var]]
    }

  } else {
    pipe_el = c("data")
    if (length(group_vars_actual) > 0 && !is_row) pipe_el = c(pipe_el, paste0("dplyr::group_by(!!!dplyr::syms(c('", paste(group_vars_actual, collapse="','"), "')))"))
    pipe_el = c(pipe_el, paste0("dplyr::mutate(`", new_var, "` = ", calc_expr, ")"))
    if (length(group_vars_actual) > 0 && !is_row) pipe_el = c(pipe_el, "dplyr::ungroup()")
    data = eval(parse(text = paste(pipe_el, collapse = " %>% ")), envir = list(data = data), enclos = parent.frame())
  }
  
  # For rank, group, tag, where if_cond/in_range usually creates NA for unselected rows
  if (func_name %in% c("rank", "group", "tag")) {
     data[[new_var]] = dplyr::if_else(data$.stata_temp_mask, data[[new_var]], NA_real_)
  }
  
  data$.stata_temp_mask = NULL
  return(data)
}
```
!END_MODIFICATION scmd_egen t_egen.R
