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
