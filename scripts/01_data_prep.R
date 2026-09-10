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

# 5. Check for duplicate rows
sum(duplicated(raw_data))

# 6. Sanity check: Bookings with 0 guests (adults + children == 0)
raw_data %>% 
  filter(number_of_adults + number_of_children == 0) %>% 
  nrow()

# 7. Sanity check: Free or negative room prices
raw_data %>% 
  filter(average_price <= 0) %>% 
  nrow()

# 8. Data Type Formatting & Cleaning Anomaly Handling
clean_data <- raw_data %>% 
  mutate(
    date_of_reservation = mdy(date_of_reservation),
    booking_status = as.factor(booking_status),
    type_of_meal = as.factor(type_of_meal),
    room_type = as.factor(room_type),
    market_segment_type = as.factor(market_segment_type)
  ) %>% 
  # Drop 37 records with unparseable/invalid reservation dates
  filter(!is.na(date_of_reservation))

# 9. Verify transformed structure and row count (should be 36,248)
glimpse(clean_data)
nrow(clean_data)

# 10. Categorical Level Audit (Catching implicit missing values like "Not Selected")
clean_data %>% 
  select(type_of_meal, room_type, market_segment_type) %>% 
  map(table)

# 11. Numeric Boundary & Range Check (Verifying Min/Max limits)
clean_data %>% 
  select(lead_time, average_price, special_requests, number_of_adults) %>% 
  summary()

# 12. Column Variance Check (Ensuring no constant single-value columns)
clean_data %>% 
  summarise(across(everything(), n_distinct)) %>% 
  pivot_longer(cols = everything(), names_to = "column", values_to = "unique_values")