!MODIFICATION drf_paths.R
scope = "file"
file = "R/drf_paths.R"
is_new_file = false
description = "Optimize Stata code parsing by checking data modification commands globally per root block instead of per-path."
---
```r
# part of DAP function
#
# Here we make the first part: extraction of data paths
# Notation is a bit old and not the same as in dap.R
#
# step: refers to a row in run_df
# node: refers to an analysis step (astep)


drf_make_paths = function(drf) {
  restore.point("drf_make_paths")
  run_df = drf$run_df; project_dir = drf$project_dir
  pid = drf$pid
  if (is.null(drf$pid)) {
    stop("Please specify drf$pid, the analyis runid of run_df.")
  }

  type_vec = drf_stata_cmd_types_vec()

  run_df = run_df %>%
    mutate(
      cmd_type = type_vec[cmd]
    )

  # Split run_df by root_file_path and compute path_df
  # Then merge path_df again for all run_df
  srun_li = split(run_df,run_df$root_file_path)
  path_li = lapply(srun_li, find_one_root_data_paths, pid=pid)
  path_df = bind_rows(path_li)

  if (NROW(path_df)==0) {
    cat("\nNo regression command found.")
    return(NULL)
  }
  return(path_df)
}


find_one_root_data_paths = function(srun_df, pid) {
  restore.point("find_one_root_data_paths")

  srun_df$.ROW = seq_len(NROW(srun_df))
  srun_df = add_load_blocks_to_run_df(srun_df)

  # --- OPTIMIZATION: Compute data modification flags globally ONCE ---
  # We check the entire run block to resolve dependencies accurately and quickly
  # rather than doing this inside the loop for every path
  stata_code = gsub("\n", " ", srun_df$cmdline, fixed = TRUE)
  cmd_df = stata2r::s2r_check_mod(stata_code)
  srun_df$is_mod = cmd_df$is_mod
  # -------------------------------------------------------------------

  spid_rows = match(pid, srun_df$runid)
  spid_rows = spid_rows[!is.na(spid_rows)]

  path_df = bind_rows(lapply(spid_rows, find_data_run_path, srun_df = srun_df, pid=pid))

  if (NROW(path_df)==0) return(NULL)
  path_df
}


extract_run_tree_from_path_df = function(root, path_df) {
  restore.point("extract_tree_from_path_df")
  tree_df = path_df %>%
    group_by(node) %>%
    mutate(
      step_ind = 1:n(),
      root_ind = match(root, dstep),
      del = is.na(root_ind) | step_ind < root_ind
    ) %>%
    ungroup() %>%
    filter(!del) %>%
    select(-step_ind, root_ind, del)
}



# FILE: repboxDRF/R/drf_paths.R
# Replace the existing find_data_run_path function:

find_data_run_path = function(pid_row, srun_df, pid=NULL) {
  restore.point("find_data_run_path")

  # All runid in same load block until pid
  path = which(srun_df$load_block == srun_df$load_block[pid_row] & srun_df$.ROW <= pid_row)

  # If we start with a restore command then jump to previous preserve
  # and then add all rows with the same load_block
  while (TRUE) {
    if (srun_df$cmd[path[1]] == "restore") {
      pr_row = srun_df$preserve_row[path[1]]
      new_path = which(srun_df$load_block == srun_df$load_block[pr_row] & srun_df$.ROW < pr_row)
      path = c(new_path, path[-1])
      next
    }
    break
  }

  # Adapt path: Utilize the globally computed is_mod flag from srun_df
  cmd_types = drf_stata_cmd_types()
  allow = c(cmd_types$scalar, cmd_types$xtset)

  keep = seq_along(path) %in% c(1, length(path)) |
    srun_df$is_mod[path] |
    srun_df$cmd[path] %in% allow |
    srun_df$runid[path] %in% pid

  path = path[keep]

  return( tibble(pid=srun_df$runid[pid_row], runid=srun_df$runid[path]))
}


# Only works for run_df that have a single rootdonum
add_load_blocks_to_run_df = function(run_df) {
  restore.point("add_load_blocks_to_run_df")
  cmd_types = drf_stata_cmd_types()
  load_cmds = cmd_types$load

  run_df$load_block = cumsum(run_df$cmd %in% c(load_cmds,"restore"))

  # Deal with preserve / restore
  run_df$preserve_row = NA
  pr_rows = which(run_df$cmd %in% c(cmd_types$preserve, cmd_types$restore))
  cur_preserve = NA
  for (r in pr_rows) {
    if (run_df$cmd[r]== "preserve") {
      cur_preserve = r
    } else if (!is.na(cur_preserve)) {
      run_df$preserve_row[r] = cur_preserve
      #rows = which(run_df$load_block == run_df$load_block[r])
      #run_df$load_block[rows] = run_df$load_block[cur_preserve]
    } else {
      wpidning("Restore command without previous preserve command is encountered!")
    }
  }
  run_df
}

#
# plot_path_df = function(path_df) {
#
#   blues_col = c("#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C")
#   reds_col = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")
#   greens_col = c("#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45")
#
#   if (!"cache" %in% colnames(path_df)) {
#     path_df$cache = FALSE
#   }
#
#   pid = path_df %>%
#     group_by(dstep) %>%
#     summarize(
#       cache = first(cache),
#       time = first(time),
#       step_type = first(step_type),
#       time = first(time),
#       time_int = 1 + (time>1) + (time > 10) + (time > 100) + (time > 1000),
#       steplab = first(steplab)
#     ) %>%
#     mutate(
#       id = 1:n(),
#       shape = ifelse(step_type=="load","dot","box"),
#       color = case_when(
#         #step_type == "load" ~ blues_col[time_int],
#         cache ~ blues_col[time_int],
#         step_type == "node" ~ greens_col[time_int],
#         TRUE ~ reds_col[time_int]
#       ),
#       label = paste0(shorten.str(steplab,30),"\n",signif(time,2)," sec."),
#       title = paste0(shorten.str(gsub("\n","<br>",steplab, fixed=TRUE),100),"<br>",signif(time,3)," sec.","<br>dstep = ",dstep)
#     )
#
#   edges = path_df %>%
#     left_join(select(pid, dstep, id), by="dstep") %>%
#     group_by(node) %>%
#     transmute(
#       from = id,
#       to = lead(id)
#     ) %>%
#     ungroup() %>%
#     filter(!is.na(to)) %>%
#     select(from, to) %>%
#     distinct() %>%
#     mutate(pidrows = "to")
#
#   title = paste0("Data-Regression Network")
#   librpidy(visNetwork)
#   vis = visNetwork(pid, edges,
#                    main = list(text=title,style="font-family: Helvetia, sans-serif;font-weight:bold;font-size:20px; text-align: center;")
#   )
#   vis
#
#
# }
```
!END_MODIFICATION drf_paths.R
