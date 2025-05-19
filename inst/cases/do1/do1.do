use "data.dta", clear
sort group i
keep in 1/39
list in 1/5
display _n
by group: gen seq = _n
gen logi = log(i)
gen sqrt_i = sqrt(i)
gen group_num = cond(group=="A",1,cond(group=="B",2,3))
egen mean_i_grp = mean(i), by(group)
egen total_i = total(i)
bysort group: egen rank_i = rank(i)
gen flag = (i>20) if group=="A"
replace flag = 0 if missing(flag)
keep if flag==1 | group=="B"
drop if i>35
collapse (mean) i (sum) total_i_sum = i, by(group)
rename i mean_i_overall
expand 2 if group=="C"
duplicates drop
encode group, gen(group_code)
decode group_code, gen(group_str)
sort group_code
tempfile t1
save "`t1'"
keep if group_code==1
merge 1:m group_code using "`t1'", keep(match master) nogenerate
append using "`t1'"
gen id = _n
bysort group_code (mean_i_overall): gen diff_mean = mean_i_overall - mean_i_overall[_n-1]
recode group_code (1=10)(2=20)(3=30)
drop if missing(mean_i_overall)
order id group_code mean_i_overall
collapse (sum) sum_mean = mean_i_overall, by(group_code)
egen total_sum = total(sum_mean)
gen proportion = sum_mean/total_sum
sort proportion
save "result_data.dta", replace
