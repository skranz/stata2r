# R Script: generate_test_data.R

# 0. Set up
set.seed(12345)
library(haven) # For writing Stata .dta files
library(dplyr) # For data manipulation convenience

# 1. Define parameters
n_ids <- 50
max_time_periods <- 5
output_dta_file <- "tricky_data.dta"
# output_do_file <- "process_tricky_data.do" # We will print this to console

# 2. Generate base data
data <- tibble(
  id = rep(1:n_ids, each = max_time_periods),
  time = rep(1:max_time_periods, times = n_ids),
  group_cat = factor(sample(1:3, n_ids * max_time_periods, replace = TRUE),
                     labels = c("Control", "TreatA", "TreatB")),
  region_cat = factor(sample(letters[1:4], n_ids * max_time_periods, replace = TRUE)),
  x_numeric = rnorm(n_ids * max_time_periods, mean = 10, sd = 2),
  another_factor = factor(sample(c("X1", "X2", "X3"), n_ids * max_time_periods, replace = TRUE)),
  baseline_char = sample(c("Low", "Medium", "High", NA_character_), n_ids * max_time_periods, replace = TRUE, prob = c(0.3, 0.3, 0.3, 0.1))
) %>%
  arrange(id, time)

# Create an outcome variable
data <- data %>%
  mutate(
    y_outcome = 5 +
      0.5 * (group_cat == "TreatA") +
      1.5 * (group_cat == "TreatB") -
      0.8 * (region_cat == "b") +
      1.2 * (region_cat == "c") +
      0.3 * x_numeric * (group_cat == "TreatA") + # Interaction
      0.7 * (another_factor == "X2") +
      rnorm(n(), 0, 1)
  )

# Introduce some more missing values strategically
data$x_numeric[sample(1:nrow(data), 5)] <- NA
data$y_outcome[sample(1:nrow(data), 3)] <- NA
data$another_factor[sample(1:nrow(data), 4)] <- NA


# Ensure some specific values for testing conditions
data$x_numeric[data$id == 1 & data$time == 1] <- 100 # For a distinct value
data$group_cat[data$id == 2 & data$time == 2] <- "TreatA"

# 3. Save data as Stata .dta file
write_dta(data, output_dta_file, version = 14)
