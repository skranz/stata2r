# FILE: R/s2r_inlist_inrange_translate.R

s2r_find_matching_paren = function(text, open_pos) {
  restore.point("s2r_find_matching_paren")
  n = stringi::stri_length(text)
  if (is.na(open_pos) || open_pos < 1 || open_pos > n) return(NA_integer_)
  if (stringi::stri_sub(text, open_pos, open_pos) != "(") return(NA_integer_)

  depth = 0L
  in_single = FALSE
  in_double = FALSE

  for (i in seq.int(open_pos, n)) {
    ch = stringi::stri_sub(text, i, i)
    prev = if (i > 1) stringi::stri_sub(text, i - 1L, i - 1L) else ""

    if (!in_double && ch == "'" && prev != "\\") {
      in_single = !in_single
      next
    }
    if (!in_single && ch == "\"" && prev != "\\") {
      in_double = !in_double
      next
    }
    if (in_single || in_double) next

    if (ch == "(") {
      depth = depth + 1L
    } else if (ch == ")") {
      depth = depth - 1L
      if (depth == 0L) return(i)
    }
  }

  NA_integer_
}

s2r_split_top_level_commas = function(text) {
  restore.point("s2r_split_top_level_commas")
  if (is.na(text) || text == "") return(character(0))

  n = stringi::stri_length(text)
  parts = character(0)
  start = 1L
  depth = 0L
  in_single = FALSE
  in_double = FALSE

  for (i in seq_len(n)) {
    ch = stringi::stri_sub(text, i, i)
    prev = if (i > 1) stringi::stri_sub(text, i - 1L, i - 1L) else ""

    if (!in_double && ch == "'" && prev != "\\") {
      in_single = !in_single
      next
    }
    if (!in_single && ch == "\"" && prev != "\\") {
      in_double = !in_double
      next
    }
    if (in_single || in_double) next

    if (ch == "(") {
      depth = depth + 1L
    } else if (ch == ")") {
      depth = depth - 1L
    } else if (ch == "," && depth == 0L) {
      parts = c(parts, stringi::stri_trim_both(stringi::stri_sub(text, start, i - 1L)))
      start = i + 1L
    }
  }

  parts = c(parts, stringi::stri_trim_both(stringi::stri_sub(text, start, n)))
  parts[!is.na(parts) & parts != ""]
}

s2r_make_local_helper_call = function(fun_name, r_args, temp_prefix) {
  restore.point("s2r_make_local_helper_call")

  if (length(r_args) == 0) {
    return(paste0(fun_name, "()"))
  }

  temp_names = paste0(temp_prefix, seq_along(r_args))
  bind_code = paste0(temp_names, " <- ", r_args)
  call_code = paste0(fun_name, "(", paste(temp_names, collapse = ", "), ")")

  paste0("local({ ", paste(c(bind_code, call_code), collapse = "; "), " })")
}

s2r_translate_inlist_call = function(args_str, context, r_value_mappings = NULL) {
  restore.point("s2r_translate_inlist_call")

  args = s2r_split_top_level_commas(args_str)
  if (length(args) == 0) {
    return("sfun_inlist()")
  }

  r_args = vapply(args, function(arg) {
    translate_stata_expression_to_r(
      arg,
      context = context,
      r_value_mappings = r_value_mappings
    )
  }, character(1))

  s2r_make_local_helper_call(
    fun_name = "sfun_inlist",
    r_args = r_args,
    temp_prefix = "s2r_inlist_arg"
  )
}

s2r_translate_inrange_call = function(args_str, context, r_value_mappings = NULL) {
  restore.point("s2r_translate_inrange_call")

  args = s2r_split_top_level_commas(args_str)
  if (length(args) == 0) {
    return("sfun_inrange()")
  }

  r_args = vapply(args, function(arg) {
    translate_stata_expression_to_r(
      arg,
      context = context,
      r_value_mappings = r_value_mappings
    )
  }, character(1))

  s2r_make_local_helper_call(
    fun_name = "sfun_inrange",
    r_args = r_args,
    temp_prefix = "s2r_inrange_arg"
  )
}

s2r_replace_special_function_calls = function(text, fun_name, replacer_fun) {
  restore.point("s2r_replace_special_function_calls")
  if (is.na(text) || text == "") return(text)

  n = stringi::stri_length(text)
  fname_n = stringi::stri_length(fun_name)

  out = ""
  last_emit = 1L
  i = 1L
  in_single = FALSE
  in_double = FALSE

  while (i <= n) {
    ch = stringi::stri_sub(text, i, i)
    prev = if (i > 1) stringi::stri_sub(text, i - 1L, i - 1L) else ""

    if (!in_double && ch == "'" && prev != "\\") {
      in_single = !in_single
      i = i + 1L
      next
    }
    if (!in_single && ch == "\"" && prev != "\\") {
      in_double = !in_double
      i = i + 1L
      next
    }

    if (!in_single && !in_double) {
      end_name = i + fname_n - 1L
      if (end_name <= n) {
        cand = stringi::stri_sub(text, i, end_name)
        next_char = if (end_name < n) stringi::stri_sub(text, end_name + 1L, end_name + 1L) else ""
        prev_char = if (i > 1L) stringi::stri_sub(text, i - 1L, i - 1L) else ""

        prev_ok = prev_char == "" || !stringi::stri_detect_regex(prev_char, "[A-Za-z0-9_]")
        next_ok = next_char == "("

        if (identical(cand, fun_name) && prev_ok && next_ok) {
          close_pos = s2r_find_matching_paren(text, end_name + 1L)
          if (is.na(close_pos)) {
            out = paste0(out, stringi::stri_sub(text, last_emit, n))
            return(out)
          }

          out = paste0(out, stringi::stri_sub(text, last_emit, i - 1L))
          inner = stringi::stri_sub(text, end_name + 2L, close_pos - 1L)
          out = paste0(out, replacer_fun(inner))

          i = close_pos + 1L
          last_emit = i
          next
        }
      }
    }

    i = i + 1L
  }

  if (last_emit <= n) {
    out = paste0(out, stringi::stri_sub(text, last_emit, n))
  }

  out
}
