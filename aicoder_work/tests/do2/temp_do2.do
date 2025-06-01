use "test_data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-1.dta", replace emptyok
generate value1_log = log(value1)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-2.dta", replace emptyok
generate value2_squared = value2^2
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-3.dta", replace emptyok
generate int_value1 = int(value1) if !missing(value1)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-4.dta", replace emptyok
generate rounded_value1 = round(value1, 0.1) if !missing(value1)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-5.dta", replace emptyok
generate random_uniform_draw = runiform()
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-6.dta", replace emptyok
generate id_plus_value2 = id + value2 if value2 < 7 & !missing(value2)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-7.dta", replace emptyok
replace value1_log = 0 if missing(value1_log) & !missing(value1)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-8.dta", replace emptyok
replace value1 = value1 * 1.5 if group_orig == "Alpha" & !missing(value1) & !missing(group_orig)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-9.dta", replace emptyok
gen group_clean = strtrim(stritrim(lower(group_orig)))
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-10.dta", replace emptyok
replace group_clean = "unknown" if missing(group_clean)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-11.dta", replace emptyok
gen group_abbr = substr(group_clean, 1, 3) if group_clean != "unknown"
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-12.dta", replace emptyok
gen contact_info = group_clean + ":" + num_str
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-13.dta", replace emptyok
gen str_len_group = strlen(group_clean)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-14.dta", replace emptyok
replace contact_info = subinstr(contact_info, "beta", "delta", 1) if strpos(contact_info, "beta") > 0
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-15.dta", replace emptyok
egen mean_overall_value1 = mean(value1)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-16.dta", replace emptyok
egen sd_overall_value1 = sd(value1)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-17.dta", replace emptyok
egen total_value2_by_group = total(value2), by(group_clean)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-18.dta", replace emptyok
egen median_value1_by_group = median(value1), by(group_clean)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-19.dta", replace emptyok
egen group_numeric_id = group(group_clean)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-20.dta", replace emptyok
egen tag_first_in_group = tag(group_clean)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-21.dta", replace emptyok
egen count_obs_in_group = count(id), by(group_clean)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-22.dta", replace emptyok
egen rank_value1_in_group = rank(value1), by(group_clean)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-23.dta", replace emptyok
egen row_total_v1_v2 = rowtotal(value1 value2)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-24.dta", replace emptyok
egen row_mean_v1_v2 = rowmean(value1 value2)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-25.dta", replace emptyok
egen concat_group_num = concat(group_clean num_str)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-26.dta", replace emptyok
summarize value2, meanonly
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-27.dta", replace emptyok
gen value2_dev_from_mean = value2 - r(mean) if !missing(value2) & r(mean) != .
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-28.dta", replace emptyok
gen obs_date = date(date_str, "YMD", 2050)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-29.dta", replace emptyok
format obs_date %tdCY-N-D
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-30.dta", replace emptyok
gen obs_year = year(obs_date) if !missing(obs_date)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-31.dta", replace emptyok
gen obs_month = month(obs_date) if !missing(obs_date)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-32.dta", replace emptyok
gen obs_day = day(obs_date) if !missing(obs_date)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-33.dta", replace emptyok
gen obs_quarter = qofd(obs_date) if !missing(obs_date)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-34.dta", replace emptyok
gen days_since_2021_start = obs_date - mdy(1,1,2021) if !missing(obs_date)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-35.dta", replace emptyok
gen is_weekend_day = (dow(obs_date) == 0 | dow(obs_date) == 6) if !missing(obs_date)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-36.dta", replace emptyok
destring num_str, generate(num_val_from_str) ignore("error")
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-37.dta", replace emptyok
replace num_val_from_str = 0 if missing(num_val_from_str)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-38.dta", replace emptyok
destring mixed_var, generate(mixed_val_num) force
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-39.dta", replace emptyok
gen mixed_var_as_str = string(mixed_val_num) if !missing(mixed_val_num)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-40.dta", replace emptyok
encode group_clean, generate(group_code_num)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-41.dta", replace emptyok
label define group_label_map 1 "alpha" 2 "beta" 3 "gamma" 4 "unknown", replace
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-42.dta", replace emptyok
label values group_code_num group_label_map
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-43.dta", replace emptyok
label variable group_code_num "Numeric code for cleaned group"
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-44.dta", replace emptyok
decode group_code_num, generate(group_from_decode)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-45.dta", replace emptyok
recode value2 (0/1=1 "Very Low") (2/4=2 "Low-Mid") (nonmissing=3 "High") (missing = .a), gen(value2_cat_str)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-46.dta", replace emptyok
recode value2 (0/1=1) (2/4=2) (nonmissing=3) (missing = 99), gen(value2_cat_num)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-47.dta", replace emptyok
sort group_clean value1
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-48.dta", replace emptyok
gsort -value1 +id
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-49.dta", replace emptyok
drop random_uniform_draw int_value1 rounded_value1
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-50.dta", replace emptyok
keep if value2 < 12 | missing(value2)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-51.dta", replace emptyok
keep if _n <= 40
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-52.dta", replace emptyok
drop if mod(id, 4) == 0 & value1 > 50
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-53.dta", replace emptyok
keep id group_orig value1 value1_log value2 value2_squared group_clean obs_date num_val_from_str value2_cat_str mean_overall_value1 total_value2_by_group group_code_num
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-54.dta", replace emptyok
rename group_orig orig_group_name
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-55.dta", replace emptyok
rename value1_log log_val1
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-56.dta", replace emptyok
order id orig_group_name group_clean log_val1
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-57.dta", replace emptyok
gen high_value1_indicator = (value1 > mean_overall_value1) if !missing(value1) & !missing(mean_overall_value1)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-58.dta", replace emptyok
gen observation_seq_num = _n
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-59.dta", replace emptyok
gen total_obs_count = _N
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-60.dta", replace emptyok
replace value1 = cond(missing(value1), mean_overall_value1, cond(value1 > 60, value1*1.05, value1*0.95))
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-61.dta", replace emptyok
compress
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-62.dta", replace emptyok
save "main_data_temp.dta", replace
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-63.dta", replace emptyok
use "extra_data.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-64.dta", replace emptyok
rename category_code extra_cat_code
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-65.dta", replace emptyok
sort id
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-66.dta", replace emptyok
save "extra_data_temp.dta", replace
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-67.dta", replace emptyok
use "main_data_temp.dta", clear
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-68.dta", replace emptyok
sort id
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-69.dta", replace emptyok
merge 1:1 id using "extra_data_temp.dta", nogen
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-70.dta", replace emptyok
append using "append_data.dta", generate(appended_flag)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-71.dta", replace emptyok
preserve
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-72.dta", replace emptyok
keep if id <= 20 & !missing(value1) & !missing(value2) & !missing(group_code_num)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-73.dta", replace emptyok
drop if missing(id)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-74.dta", replace emptyok
bysort id group_code_num: gen time_var_for_reshape = 100 + _n
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-75.dta", replace emptyok
keep if time_var_for_reshape <= 102
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-76.dta", replace emptyok
duplicates drop id group_code_num time_var_for_reshape, force
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-77.dta", replace emptyok
reshape wide value1 value2, i(id group_code_num) j(time_var_for_reshape)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-78.dta", replace emptyok
reshape long value1 value2, i(id group_code_num) j(time_var_reverted)
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-79.dta", replace emptyok
restore
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-80.dta", replace emptyok
label data "Comprehensive Test Data - Final Version"
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-81.dta", replace emptyok
save "result_data.dta"
save "/home/rstudio/aicoder/stata2r/aicoder_work/tests/do2/do_data/do2-82.dta", replace emptyok
