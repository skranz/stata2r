use "tricky_data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-1.dta", replace emptyok
label var id "Individual ID"
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-2.dta", replace emptyok
quietly regress y_outcome x_numeric
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-3.dta", replace emptyok
capture gen _temp_eN_reg1 = e(N)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-4.dta", replace emptyok
gen obs_from_reg1 = _temp_eN_reg1
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-5.dta", replace emptyok
bysort id: replace obs_from_reg1 = cond(_n == 1, obs_from_reg1, .)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-6.dta", replace emptyok
gen _temp_df_r_reg1 = e(df_r)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-7.dta", replace emptyok
gen df_resid_from_reg1 = _temp_df_r_reg1
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-8.dta", replace emptyok
bysort id: replace df_resid_from_reg1 = cond(_n == 1, df_resid_from_reg1, .)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-9.dta", replace emptyok
gen in_estimation_sample1 = e(sample)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-10.dta", replace emptyok
assert in_estimation_sample1 == 1 | in_estimation_sample1 == 0 | missing(in_estimation_sample1)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-11.dta", replace emptyok
gen y_mod_by_esample1 = y_outcome
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-12.dta", replace emptyok
gen _cond_ymod1 = (in_estimation_sample1 == 1 & group_cat == 2)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-13.dta", replace emptyok
gen _new_val_ymod1 = y_outcome * 1.1
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-14.dta", replace emptyok
replace y_mod_by_esample1 = cond(_cond_ymod1, _new_val_ymod1, y_mod_by_esample1)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-15.dta", replace emptyok
xi i.group_cat
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-16.dta", replace emptyok
summarize _Igroup_cat_2 _Igroup_cat_3
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-17.dta", replace emptyok
gen _cond_xw = (_Igroup_cat_2 == 1 & !missing(_Igroup_cat_2))
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-18.dta", replace emptyok
gen x_weighted_by_Igroup2 = cond(_cond_xw, x_numeric * _Igroup_cat_2, .)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-19.dta", replace emptyok
drop _Igroup_cat*
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-20.dta", replace emptyok
xi i.another_factor
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-21.dta", replace emptyok
gen _interaction_another_f_2_xnum = _Ianother_f_2 * x_numeric
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-22.dta", replace emptyok
drop _Ianother_f* _interaction_another_f*
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-23.dta", replace emptyok
xi i.region_cat*i.group_cat
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-24.dta", replace emptyok
summarize _IregXgro_2_2 _IregXgro_3_3
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-25.dta", replace emptyok
gen y_if_xi_interaction_specific = cond((_IregXgro_2_2 == 1), y_outcome * 100, .)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-26.dta", replace emptyok
drop  _Igroup_cat* _IregXgro_2_2 _IregXgro_3_3
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do3/do_data/do3-27.dta", replace emptyok
