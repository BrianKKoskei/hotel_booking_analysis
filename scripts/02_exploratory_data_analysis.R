library(tidyverse)

# 1. Load cleaned dataset directly from RDS
clean_data <- read_rds("data/processed/clean_booking.rds")

# 2. Key Metric Summary: Lead Time & Pricing by Cancellation Status
eda_summary <- clean_data %>% 
  group_by(booking_status) %>% 
  summarise(
    count = n(),
    avg_lead_time = mean(lead_time),
    median_lead_time = median(lead_time),
    avg_price = mean(average_price),
    median_price = median(average_price)
  )

print(eda_summary)

# 3. Visualization: Lead Time Boxplot by Status
ggplot(clean_data, aes(x = booking_status, y = lead_time, fill = booking_status)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.alpha = 0.3) +
  theme_minimal() +
  labs(
    title = "Lead Time Distribution by Booking Status",
    x = "Booking Status",
    y = "Lead Time (Days)"
  )

# 4. Pricing Analysis across Market Segments
price_summary <- clean_data %>% 
  group_by(market_segment_type, booking_status) %>% 
  summarise(
    count = n(),
    avg_price = mean(average_price),
    median_price = median(average_price),
    .groups = "drop"
  )

print(price_summary)

# 5. Visualization: Price Distribution by Market Segment & Status
ggplot(clean_data, aes(x = market_segment_type, y = average_price, fill = booking_status)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.2) +
  theme_minimal() +
  labs(
    title = "Average Room Price by Market Segment and Booking Status",
    x = "Market Segment",
    y = "Average Price (€)"
  )

# 6. Special Requests vs Booking Status
request_summary <- clean_data %>% 
  group_by(booking_status) %>% 
  summarise(
    total_bookings = n(),
    avg_special_requests = mean(special_requests),
    pct_with_requests = mean(special_requests > 0) * 100
  )

print(request_summary)

# 7. Visualization: Special Requests Proportion
ggplot(clean_data, aes(x = factor(special_requests), fill = booking_status)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(
    title = "Booking Status Proportion by Special Requests Count",
    x = "Number of Special Requests",
    y = "Proportion"
  )

# 8. Parking Space Requirements vs Booking Status
parking_summary <- clean_data %>% 
  group_by(car_parking_space, booking_status) %>% 
  summarise(
    count = n(),
    .groups = "drop"
  ) %>% 
  group_by(car_parking_space) %>% 
  mutate(percentage = count / sum(count) * 100)

print(parking_summary)