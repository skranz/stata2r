use "data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-1.dta", replace emptyok
xi: regress y_outcome i.group_cat x_numeric
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-2.dta", replace emptyok
reg y x
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-3.dta", replace emptyok
reg y x i.group_cat
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-4.dta", replace emptyok
keep if e(sample)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-5.dta", replace emptyok
egen active_dummy_count = rowtotal(_Igroup_cat_2 _Igroup_cat_3)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-6.dta", replace emptyok
gen treated_obs = (_Igroup_cat_2 == 1 | _Igroup_cat_3 == 1)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-7.dta", replace emptyok
keep if active_dummy_count > 0 | x_numeric > 5
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-8.dta", replace emptyok
bysort group_cat: egen mean_treated = mean(treated_obs)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-9.dta", replace emptyok
order y_outcome group_cat x_numeric _Igroup_cat_2 _Igroup_cat_3 active_dummy_count treated_obs mean_tre
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-10.dta", replace emptyok
use "data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-11.dta", replace emptyok
keep ti id group_c
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-12.dta", replace emptyok
keep if id >= 1
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-13.dta", replace emptyok
keep if ti >= log(5)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-14.dta", replace emptyok
use "data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-15.dta", replace emptyok
keep in 1/6
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-16.dta", replace emptyok
use "data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-17.dta", replace emptyok
keep if tim <= id / 2
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-18.dta", replace emptyok
use "data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-19.dta", replace emptyok
keep if id <= 3
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-20.dta", replace emptyok
use "data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-21.dta", replace emptyok
drop group_c
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-22.dta", replace emptyok
drop if id <= 1
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-23.dta", replace emptyok
drop if ti >= log(5)+id
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-24.dta", replace emptyok
use "data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-25.dta", replace emptyok
drop in 1/6
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-26.dta", replace emptyok
use "data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-27.dta", replace emptyok
drop if tim >= id / 2
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-28.dta", replace emptyok
use "data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-29.dta", replace emptyok
drop if id <= 3
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-30.dta", replace emptyok
