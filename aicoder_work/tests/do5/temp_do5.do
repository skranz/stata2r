use "data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-1.dta", replace emptyok
keep ti id group_c
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-2.dta", replace emptyok
keep if id >= 1
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-3.dta", replace emptyok
keep if ti >= log(5)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-4.dta", replace emptyok
use "data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-5.dta", replace emptyok
keep in 1/6
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-6.dta", replace emptyok
use "data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-7.dta", replace emptyok
keep if tim <= id / 2
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-8.dta", replace emptyok
use "data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-9.dta", replace emptyok
keep if id <= 3
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-10.dta", replace emptyok
use "data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-11.dta", replace emptyok
drop group_c
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-12.dta", replace emptyok
drop if id <= 1
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-13.dta", replace emptyok
drop if ti >= log(5)+id
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-14.dta", replace emptyok
use "data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-15.dta", replace emptyok
drop in 1/6
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-16.dta", replace emptyok
use "data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-17.dta", replace emptyok
drop if tim >= id / 2
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-18.dta", replace emptyok
use "data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-19.dta", replace emptyok
drop if id <= 3
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do5/do_data/do5-20.dta", replace emptyok
