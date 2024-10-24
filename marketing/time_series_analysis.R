# Load necessary libraries
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(prophet)  # Add Prophet library

# Import data_loading.R
source("marketing/data_loading.R")

# Find the range of dates
date_range <- range(as.Date(merged_data$InvoiceDate))
print(paste("Date Range:", date_range[1], "to", date_range[2]))

# -------------------------------
# 12. Time Series Analysis
# -------------------------------
print("Performing Time Series Analysis...")

# Aggregate Sales Over Time (Monthly)
monthly_sales <- merged_data %>%
  mutate(InvoiceMonth = format(as.Date(InvoiceDate), "%Y-%m")) %>%
  group_by(InvoiceMonth) %>%
  summarise(Total_Sales = sum(UnitPrice * Quantity, na.rm = TRUE), .groups = 'drop') %>%
  arrange(InvoiceMonth)

# Display first few rows to verify
print("Monthly Sales:")
print(head(monthly_sales))

# Define the complete range of months based on actual data, excluding incomplete last month
start_month <- as.Date("2010-12-01")
end_month <- as.Date("2011-11-01")  # Changed to exclude December
all_months <- seq(start_month, end_month, by = "month")

monthly_sales <- monthly_sales %>%
  complete(InvoiceMonth = format(all_months, "%Y-%m"), fill = list(Total_Sales = 0))

# Display all months to verify completeness
print("All Months in Data:")
print(monthly_sales)

# Convert InvoiceMonth to Date format
monthly_sales$InvoiceMonth <- as.Date(paste0(monthly_sales$InvoiceMonth, "-01"), "%Y-%m-%d")

# Filter data to ensure it doesn't go past 2011-11-30
end_date <- as.Date("2011-11-30")
monthly_sales_clean <- monthly_sales[monthly_sales$InvoiceMonth <= end_date, ]

# Plot the cleaned data
ggplot(monthly_sales_clean, aes(x = InvoiceMonth, y = Total_Sales)) +
  geom_line(color = "blue") +
  labs(title = "Historical Monthly Sales (Cleaned)",
       x = "Month",
       y = "Total Sales") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_y_continuous(labels = scales::comma)

# Prepare data for Prophet
monthly_sales_clean <- monthly_sales_clean %>%
  rename(ds = InvoiceMonth, y = Total_Sales)

# Fit the Prophet model with even lower changepoint prior scale
prophet_model <- prophet(
  monthly_sales_clean,
  growth = 'linear',
  yearly.seasonality = FALSE,
  weekly.seasonality = FALSE,
  daily.seasonality = FALSE,
  seasonality.mode = 'additive',
  changepoint.prior.scale = 0.01,   # Further reduced from 0.05
  n.changepoints = 2                 # Further reduced from 3
)

# Forecast Future Sales Using Prophet
future <- make_future_dataframe(prophet_model, periods = 3, freq = 'month')
forecast <- predict(prophet_model, future)

# Ensure forecasted values are non-negative
forecast$yhat <- pmax(forecast$yhat, 0)

# Ensure the 'ds' column is in Date format
forecast$ds <- as.Date(forecast$ds)

# Filter out incomplete data for the last month (already excluded)

# Plot the Forecast
forecast_plot_prophet <- ggplot() +
  geom_line(data = forecast, aes(x = ds, y = yhat), color = "red") +
  geom_line(data = monthly_sales_clean, aes(x = ds, y = y), color = "blue") +
  labs(title = "Sales Forecast for Next 3 Months (Prophet)",
       x = "Date",
       y = "Sales") +
  theme_minimal(base_size = 15) +
  scale_y_continuous(labels = scales::comma) +  # Ensure no scientific notation
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save the improved plot
ggsave("plots/sales_forecast_prophet.png", plot = forecast_plot_prophet)

