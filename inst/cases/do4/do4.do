use "tricky_data.dta", clear
transform data
quietly regress y_outcome time
quietly regress y_outcome x_numeric
summarize x_numeric y_outcome
keep if e(sample)
summarize x_numeric y_outcome
