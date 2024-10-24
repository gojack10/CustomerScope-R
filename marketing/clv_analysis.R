library(dplyr)
library(CLVTools)
library(readr)

# Source the data_loading.R script to load and prepare data
source("marketing/data_loading.R")

# Load cluster profiles
print("Loading Cluster Profiles...")
cluster_profiles <- read_csv("data/cluster_profiles.csv")

# Display first few rows to verify
print("Cluster Profiles Summary:")
print(head(cluster_profiles))

# Prepare data for CLVTools
print("Preparing Data for CLVTools...")

# Define buffer period (in days) before the last transaction date
buffer_days <- 2

# Calculate the last transaction date in the dataset
last_date <- max(merged_data$InvoiceDate)

# Calculate the maximum first transaction date across all customers
max_first_transaction <- merged_data %>%
  group_by(CustomerID) %>%
  summarise(first_transaction = min(InvoiceDate)) %>%
  ungroup() %>%
  summarise(max_first_txn = max(first_transaction)) %>%
  pull(max_first_txn)

# Determine the estimation split
estimation_split <- min(last_date - buffer_days, max_first_transaction)

# Print the determined estimation split
print(paste("Determined Estimation Split:", estimation_split))

# Identify customers with first_transaction <= estimation_split
valid_customers <- merged_data %>%
  group_by(CustomerID) %>%
  summarise(first_transaction = min(InvoiceDate)) %>%
  ungroup() %>%
  filter(first_transaction <= estimation_split) %>%
  pull(CustomerID)

# Number of valid customers
print(paste("Number of Valid Customers:", length(valid_customers)))

# Filter the merged_data to include only valid customers
filtered_data <- merged_data %>%
  filter(CustomerID %in% valid_customers)

# Check if any customers are excluded
excluded_customers <- merged_data %>%
  filter(!CustomerID %in% valid_customers) %>%
  distinct(CustomerID)

if(nrow(excluded_customers) > 0) {
  warning(paste("Excluding", nrow(excluded_customers), "customers whose first transaction is after the estimation split."))
} else {
  print("All customers are included in the CLV analysis.")
}

# Create CLV data using the filtered dataset
clv_data <- clvdata(
  data.transactions = filtered_data,
  date.format = "ymd",
  time.unit = "days",
  estimation.split = estimation_split,
  name.id = "CustomerID",
  name.date = "InvoiceDate",
  name.price = "UnitPrice"
)

# Fit the BG/NBD model
print("Fitting BG/NBD Model...")
bg_nbd_model <- bgnbd(clv_data)

# Predict CLV for the next 12 periods
print("Predicting CLV for the Next 12 Periods...")
clv <- predict(bg_nbd_model, clv_data, prediction.end = 12)

# Filter customer_metrics to include only valid customers
filtered_customer_metrics <- customer_metrics %>%
  filter(CustomerID %in% valid_customers)

# Convert CLV to a Data Frame using the filtered customer metrics
clv_df <- data.frame(CustomerID = filtered_customer_metrics$CustomerID, CLV = clv)

# Output the CLV estimation results
print("CLV Estimation Results:")
print(head(clv_df))

# Save the results to a CSV file
write.csv(clv_df, "data/clv_estimation_results.csv", row.names = FALSE)
