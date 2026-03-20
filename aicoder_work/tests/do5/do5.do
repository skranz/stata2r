use "data.dta", clear
xi: regress y_outcome i.group_cat x_numeric
reg y x
reg y x i.group_cat
keep if e(sample)
egen active_dummy_count = rowtotal(_Igroup_cat_2 _Igroup_cat_3)
gen treated_obs = (_Igroup_cat_2 == 1 | _Igroup_cat_3 == 1)
keep if active_dummy_count > 0 | x_numeric > 5
bysort group_cat: egen mean_treated = mean(treated_obs)
order y_outcome group_cat x_numeric _Igroup_cat_2 _Igroup_cat_3 active_dummy_count treated_obs mean_tre
use "data.dta", clear
keep ti id group_c
keep if id >= 1
keep if ti >= log(5)
use "data.dta", clear
keep in 1/6
use "data.dta", clear
keep if tim <= id / 2
use "data.dta", clear
keep if id <= 3
use "data.dta", clear
drop group_c
drop if id <= 1
drop if ti >= log(5)+id
use "data.dta", clear
drop in 1/6
use "data.dta", clear
drop if tim >= id / 2
use "data.dta", clear
drop if id <= 3
