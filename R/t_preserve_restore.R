# FILE: R/t_preserve_restore.R

# 1. Parsing Phase
s2r_p_preserve_restore = function(cmd_obj, type) {
  list(type = type)
}

# 2. Code Generation Phase
t_preserve_restore = function(cmd_obj, type = "preserve") {
  restore.point("t_preserve_restore")
  if (type == "preserve") {
    return("data = scmd_preserve(data)")
  } else {
    return("data = scmd_restore()")
  }
}

# 3. Runtime Execution Phase
scmd_preserve = function(data) {
  restore.point("scmd_preserve")
  if (!exists("preserve_stack", envir = stata2r_env)) stata2r_env$preserve_stack = list()
  stata2r_env$preserve_stack = c(list(data), stata2r_env$preserve_stack)
  return(data)
}

scmd_restore = function() {
  restore.point("scmd_restore")
  if (!exists("preserve_stack", envir = stata2r_env) || length(stata2r_env$preserve_stack) == 0) {
    warning("Stata restore called but preserve stack is empty.")
    return(data) # Fallback if error mapping is missing
  }

  restored_data = stata2r_env$preserve_stack[[1]]
  stata2r_env$preserve_stack = stata2r_env$preserve_stack[-1]

  if (isTRUE(stata2r_env$has_original_order_idx)) {
    restored_data$stata2r_original_order_idx = seq_len(nrow(restored_data))
  }

  return(restored_data)
}
