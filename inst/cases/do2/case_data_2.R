# R code to create the initial Stata datasets
# Ensure foreign package is available
library(foreign)

set.seed(123) # for reproducibility

# Main test data
data = data.frame(
  id = 1:50,
  group_orig = sample(c("Alpha", "Beta ", " Gamma", "Alpha", "Beta "), 50, replace = TRUE),
  value1 = runif(50, 10, 100),
  value2 = rpois(50, 5),
  date_str = paste0(sample(2020:2022, 50, replace=TRUE), "-",
                    sprintf("%02d", sample(1:12, 50, replace=TRUE)), "-",
                    sprintf("%02d", sample(1:28, 50, replace=TRUE))),
  num_str = as.character(sample(100:200, 50, replace=TRUE)),
  mixed_var = sample(c("10", "20", "apple", "30", "banana", "42", "55"), 50, replace = TRUE)
)
# Introduce some NAs
data$value1[sample(1:50, 5)] <- NA
data$group_orig[sample(1:50, 3)] <- NA
data$value2[sample(1:50, 2)] <- NA
data$date_str[sample(1:50, 2)] <- NA

write.dta(data, "test_data.dta", version = 12) # Stata 12 format for wider compatibility

# Second dataset for merge
set.seed(456)
data2 = data.frame(
  id = sample(30:70, 30, replace = FALSE), # some overlapping, some new ids
  extra_info_val = runif(30, 0, 1),
  category_code = sample(c("X1", "Y2", "Z3", "X1"), 30, replace = TRUE)
)
write.dta(data2, "extra_data.dta", version = 12)

# Third dataset for append (different structure)
set.seed(789)
data_append = data.frame(
  id = 71:80, # new ids
  group_orig = sample(c("Delta", "Epsilon"), 10, replace = TRUE),
  value1 = runif(10, 200, 300)
  # value2, date_str, num_str, mixed_var are missing to test append behavior
)
write.dta(data_append, "append_data.dta", version=12)

cat("Stata .dta files (test_data.dta, extra_data.dta, append_data.dta) created.\n")
