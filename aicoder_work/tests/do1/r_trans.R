data = haven::read_dta("data.dta") # 'clear' was used
data = collapse::setorder(data, group, i) # Using collapse::setorder for efficiency
data = collapse::fgroup_by(data, c("group"))
data = collapse::fmutate(data, seq = dplyr::row_number())
data = collapse::fungroup(data)
data = collapse::fmutate(data, logi = log(i))
data = collapse::fmutate(data, sqrt_i = sqrt(i))
data = collapse::fmutate(data, group_num = dplyr::if_else(group=="A", 1, cond(group=="B",2,3)))
data = data %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(mean_i_grp = mean(i, na.rm = TRUE)) %>%
  dplyr::ungroup()
data = dplyr::mutate(data, total_i = sum(i, na.rm = TRUE))
data = data %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(rank_i = dplyr::min_rank(i)) %>%
  dplyr::ungroup()
data = collapse::fmutate(data, flag = dplyr::if_else(group=="A", (i>20), NA_real_))
data = collapse::fmutate(data, flag = dplyr::if_else(is.na(flag), 0, flag))
data = collapse::fsubset(data, flag==1 | group=="B")
data = collapse::fsubset(data, !(i>35))
