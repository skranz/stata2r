use "test_data.dta", clear
generate value1_log = log(value1)
generate value2_squared = value2^2
generate int_value1 = int(value1) if !missing(value1)
generate rounded_value1 = round(value1, 0.1) if !missing(value1)
generate random_uniform_draw = runiform()
generate id_plus_value2 = id + value2 if value2 < 7 & !missing(value2)
replace value1_log = 0 if missing(value1_log) & !missing(value1)
replace value1 = value1 * 1.5 if group_orig == "Alpha" & !missing(value1) & !missing(group_orig)
gen group_clean = strtrim(stritrim(lower(group_orig)))
replace group_clean = "unknown" if missing(group_clean)
gen group_abbr = substr(group_clean, 1, 3) if group_clean != "unknown"
gen contact_info = group_clean + ":" + num_str
gen str_len_group = strlen(group_clean)
replace contact_info = subinstr(contact_info, "beta", "delta", 1) if strpos(contact_info, "beta") > 0
egen mean_overall_value1 = mean(value1)
egen sd_overall_value1 = sd(value1)
egen total_value2_by_group = total(value2), by(group_clean)
egen median_value1_by_group = median(value1), by(group_clean)
egen group_numeric_id = group(group_clean)
egen tag_first_in_group = tag(group_clean)
egen count_obs_in_group = count(id), by(group_clean)
egen rank_value1_in_group = rank(value1), by(group_clean)
egen row_total_v1_v2 = rowtotal(value1 value2)
egen row_mean_v1_v2 = rowmean(value1 value2)
egen concat_group_num = concat(group_clean num_str)
summarize value2, meanonly
gen value2_dev_from_mean = value2 - r(mean) if !missing(value2) & r(mean) != .
gen obs_date = date(date_str, "YMD", 2050)
format obs_date %tdCY-N-D
gen obs_year = year(obs_date) if !missing(obs_date)
gen obs_month = month(obs_date) if !missing(obs_date)
gen obs_day = day(obs_date) if !missing(obs_date)
gen obs_quarter = qofd(obs_date) if !missing(obs_date)
gen days_since_2021_start = obs_date - mdy(1,1,2021) if !missing(obs_date)
gen is_weekend_day = (dow(obs_date) == 0 | dow(obs_date) == 6) if !missing(obs_date)
destring num_str, generate(num_val_from_str) ignore("error")
replace num_val_from_str = 0 if missing(num_val_from_str)
destring mixed_var, generate(mixed_val_num) force
gen mixed_var_as_str = string(mixed_val_num) if !missing(mixed_val_num)
encode group_clean, generate(group_code_num)
label define group_label_map 1 "alpha" 2 "beta" 3 "gamma" 4 "unknown", replace
label values group_code_num group_label_map
label variable group_code_num "Numeric code for cleaned group"
decode group_code_num, generate(group_from_decode)
recode value2 (0/1=1 "Very Low") (2/4=2 "Low-Mid") (nonmissing=3 "High") (missing = .a), gen(value2_cat_str)
recode value2 (0/1=1) (2/4=2) (nonmissing=3) (missing = 99), gen(value2_cat_num)
sort group_clean value1
gsort -value1 +id
drop random_uniform_draw int_value1 rounded_value1
keep if value2 < 12 | missing(value2)
keep if _n <= 40
drop if mod(id, 4) == 0 & value1 > 50
keep id group_orig value1 value1_log value2 value2_squared group_clean obs_date num_val_from_str value2_cat_str mean_overall_value1 total_value2_by_group group_code_num
rename group_orig orig_group_name
rename value1_log log_val1
order id orig_group_name group_clean log_val1
gen high_value1_indicator = (value1 > mean_overall_value1) if !missing(value1) & !missing(mean_overall_value1)
gen observation_seq_num = _n
gen total_obs_count = _N
replace value1 = cond(missing(value1), mean_overall_value1, cond(value1 > 60, value1*1.05, value1*0.95))
compress
save "main_data_temp.dta", replace
use "extra_data.dta", clear
rename category_code extra_cat_code
sort id
save "extra_data_temp.dta", replace
use "main_data_temp.dta", clear
sort id
merge 1:1 id using "extra_data_temp.dta", nogen
append using "append_data.dta", generate(appended_flag)
preserve
keep if id <= 20 & !missing(value1) & !missing(value2) & !missing(group_code_num)
drop if missing(id)
bysort id group_code_num: gen time_var_for_reshape = 100 + _n
keep if time_var_for_reshape <= 102
duplicates drop id group_code_num time_var_for_reshape, force
reshape wide value1 value2, i(id group_code_num) j(time_var_for_reshape)
reshape long value1 value2, i(id group_code_num) j(time_var_reverted)
restore
label data "Comprehensive Test Data - Final Version"
save "result_data.dta"
