data = haven::read_dta("data.dta") # 'clear' was used
data = collapse::setorder(data, group, i) # Using collapse::setorder for efficiency
data = collapse::fgroup_by(data, c("group"))
data = collapse::fmutate(data, seq = dplyr::row_number())
data = collapse::fungroup(data)
data = collapse::fmutate(data, logi = log(i))
data = collapse::fmutate(data, sqrt_i = sqrt(i))
data = collapse::fmutate(data, group_num = dplyr::if_else(group=="A", 1, cond(group=="B",2,3)))
data = collapse::fgroup_by(data, c("group")) %>%
  collapse::fmutate(mean_i_grp = mean(i, na.rm = TRUE)) %>%
  collapse::fungroup()
data = collapse::fmutate(data, total_i = sum(i, na.rm = TRUE))
data = collapse::fgroup_by(data, c("group")) %>%
  collapse::fmutate(rank_i = dplyr::min_rank(i)) %>%
  collapse::fungroup()
data = collapse::fmutate(data, flag = dplyr::if_else(group=="A", (i>20), NA_real_))
data = collapse::fmutate(data, flag = dplyr::if_else(is.na(flag), 0, flag))
data = collapse::fsubset(data, flag==1 | group=="B")
data = collapse::fsubset(data, !(i>35))
data = collapse::fgroup_by(data, c("group")) %>%
  collapse::fsummarise(i = collapse::fmean(i, na.rm = TRUE),
  total_i_sum = collapse::fsum(i, na.rm = TRUE)) %>%
  collapse::fungroup()
data = collapse::frename(data, `i` = `mean_i_overall`)
__expand_n_values_L16 = 2
__expand_cond_values_L16 = group=="C"
__final_expand_times_L16 = ifelse(!is.na(__expand_cond_values_L16) & __expand_cond_values_L16, ifelse(is.na(__expand_n_values_L16), 1, pmax(0, as.integer(__expand_n_values_L16))), 1)
__final_expand_times_L16 = ifelse(is.na(__final_expand_times_L16), 1, __final_expand_times_L16)
data = data[base::rep(1:NROW(data), times = __final_expand_times_L16), ]
if (exists('__expand_n_values_L16')) rm(__expand_n_values_L16, __expand_cond_values_L16, __final_expand_times_L16)
## Calculate duplicate flag based on all variablesNA
__is_duplicate_L17 = base::duplicated(data, fromLast = FALSE)
## Calculate condition flag
__satisfies_cond_L17 = TRUE
data = base::subset(data, !(__is_duplicate_L17 & __satisfies_cond_L17))
rm(__is_duplicate_L17, __satisfies_cond_L17)
data = collapse::fmutate(data, group_code = NA_integer_)
__encoded_values_L18 = as.integer(base::factor(data$group, levels = base::unique(data$group[base::order(data$group)])))
data = collapse::fmutate(data, group_code = __encoded_values_L18)
rm(__encoded_values_L18)
data = collapse::fmutate(data, group_str = NA_character_)
## Decode values using haven::as_factor
__decoded_values_L19 = as.character(haven::as_factor(data$group_code, levels = 'labels'))
data = collapse::fmutate(data, group_str = __decoded_values_L19)
rm(__decoded_values_L19)
data = collapse::setorder(data, group_code) # Using collapse::setorder for efficiency
R_tempfile_L21_t1_path = tempfile(fileext = '.dta') # Stata tempfile 't1'
haven::write_dta(data, path = "`t1'")
data = collapse::fsubset(data, group_code==1)
data = collapse::fmerge(data, haven::read_dta("`t1'"), by = c("1:m", "group_code"), all.x = TRUE, all.y = FALSE) # Stata merge type: 1:1, keep(master), nogenerate
data = collapse::fbind(data, haven::read_dta("`t1'"))
data = tidyr::pivot_wider(data, id_cols = dplyr::all_of(c("group_code")), names_from = group_str, values_from = dplyr::all_of(c("mean_i_overall", "total_i_sum")))
# reshape long with multiple stubnames not yet fully implemented: long mean_i_overall total_i_sum, i(group_code) j(group_str)
data = collapse::fmutate(data, id = dplyr::row_number())
data = collapse::fgroup_by(data, c("group_code", "(mean_i_overall)"))
data = collapse::fmutate(data, diff_mean = mean_i_overall - mean_i_overall[dplyr::row_number()-1])
data = collapse::fungroup(data)
data = collapse::fmutate(data, group_code = dplyr::case_when(
    group_code %in% c(1) ~ 10,
    group_code %in% c(2) ~ 20,
    group_code %in% c(3) ~ 30
  ))
data = collapse::fsubset(data, !(is.na(mean_i_overall)))
data = data[1:10,]
data = dplyr::select(data, id, group_code, mean_i_overall, dplyr::everything())
data = collapse::fgroup_by(data, c("group_code")) %>%
  collapse::fsummarise(sum_mean = collapse::fsum(mean_i_overall, na.rm = TRUE)) %>%
  collapse::fungroup()
data = collapse::fmutate(data, total_sum = sum(sum_mean, na.rm = TRUE))
data = collapse::fmutate(data, proportion = sum_mean/total_sum)
data = collapse::setorder(data, proportion) # Using collapse::setorder for efficiency
NA
haven::write_dta(data, path = "result_data.dta")
