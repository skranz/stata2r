use "tricky_data.dta", clear
label var id "Individual ID"
quietly regress y_outcome x_numeric
gen _temp_eN_reg1 = e(N)
gen obs_from_reg1 = _temp_eN_reg1
bysort id: replace obs_from_reg1 = cond(_n == 1, obs_from_reg1, .)
gen _temp_df_r_reg1 = e(df_r)
gen df_resid_from_reg1 = _temp_df_r_reg1
bysort id: replace df_resid_from_reg1 = cond(_n == 1, df_resid_from_reg1, .)
gen in_estimation_sample1 = e(sample)
assert in_estimation_sample1 == 1 | in_estimation_sample1 == 0 | missing(in_estimation_sample1)
gen y_mod_by_esample1 = y_outcome
gen _cond_ymod1 = (in_estimation_sample1 == 1 & group_cat == 2)
gen _new_val_ymod1 = y_outcome * 1.1
replace y_mod_by_esample1 = cond(_cond_ymod1, _new_val_ymod1, y_mod_by_esample1)
xi i.group_cat
summarize _Igroup_cat_2 _Igroup_cat_3
gen _cond_xw = (_Igroup_cat_2 == 1 & !missing(_Igroup_cat_2))
gen x_weighted_by_Igroup2 = cond(_cond_xw, x_numeric * _Igroup_cat_2, .)
drop _Igroup_cat*
xi i.another_factor
gen _interaction_another_f_2_xnum = _Ianother_f_2 * x_numeric
gen _interaction_another_f_3_xnum = _Ianother_f_3 * x_numeric
drop _Ianother_f* _interaction_another_f*
xi i.region_cat*i.group_cat
gen y_if_xi_interaction_specific = cond((_IregXgro_2_2 == 1), y_outcome * 100, .)
drop  _Igroup_cat* _IregXgro_2_2 _IregXgro_3_3
gen log_y = log(y_outcome) if y_outcome > 0
gen sample_first100 = 1 in 1/100
drop if in_estimation_sample1 == 1 & group_cat == 1
bysort id (y_outcome): gen y_rank = _n
gen ratio_xy = x_numeric / y_outcome if y_outcome != 0
gen high_x = (x_numeric > 10) if !missing(x_numeric)
egen mean_x_by_group = mean(x_numeric), by(group_cat)
keep if in_estimation_sample1 == 1 & group_cat == 3
