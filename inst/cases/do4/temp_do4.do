use "tricky_data.dta", clear
save "/home/rstudio/aicoder/stata2r/inst/cases/do4/do_data/do4-1.dta", replace emptyok
quietly regress y_outcome time
save "/home/rstudio/aicoder/stata2r/inst/cases/do4/do_data/do4-2.dta", replace emptyok
quietly regress y_outcome x_numeric
save "/home/rstudio/aicoder/stata2r/inst/cases/do4/do_data/do4-3.dta", replace emptyok
summarize x_numeric y_outcome
save "/home/rstudio/aicoder/stata2r/inst/cases/do4/do_data/do4-4.dta", replace emptyok
keep if e(sample)
save "/home/rstudio/aicoder/stata2r/inst/cases/do4/do_data/do4-5.dta", replace emptyok
summarize x_numeric y_outcome
save "/home/rstudio/aicoder/stata2r/inst/cases/do4/do_data/do4-6.dta", replace emptyok
