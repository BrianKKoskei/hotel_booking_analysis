# Script 01: Data Preparation & Cleaning
library(tidyverse)

# 1. Load raw data and standardize column names (replacing spaces with underscores)
raw_data <- read_csv("data/raw/booking.csv") %>% 
  rename_all(~ str_replace_all(., " ", "_"))

# 2. Inspect clean column names
glimpse(raw_data)

# 3. Check for missing values (NAs)
sum(is.na(raw_data))

# 4. Target distribution (Cancellation rate)
raw_data %>% 
  count(booking_status) %>% 
  mutate(percentage = n / sum(n) * 100)