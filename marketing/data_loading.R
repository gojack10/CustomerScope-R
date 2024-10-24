 library(dplyr)
library(readxl)
library(readr)

# -------------------------------
# 1. Load Transactional Data
# -------------------------------
print("Loading Transactional Data...")
customer_data <- read_excel("data/online-retail.xlsx")

# Convert InvoiceDate to Date type
customer_data$InvoiceDate <- as.Date(customer_data$InvoiceDate, format = "%Y-%m-%d")

# -------------------------------
# 2. Load Customer Segments
# -------------------------------
print("Loading Customer Segments...")
customer_segments <- read_csv("data/customer_segments.csv")

# Display first few rows to verify
print("Customer Segments:")
print(head(customer_segments))

 library(dplyr)

# Assuming customer_data and customer_segments are loaded from data_loading.R

# -------------------------------
# 3. Merge Transactional Data with Customer Segments
# -------------------------------
print("Merging Transactional Data with Customer Segments...")
# Use inner_join to retain only matching CustomerIDs
merged_data <- customer_data %>%
  inner_join(customer_segments, by = "CustomerID")

# DEBUG
print("Merged Data:")
print(head(merged_data))
print("Last few entries in merged_data:")
print(tail(merged_data))


# Print the range of InvoiceDate in the merged_data
print(paste("Invoice Date Range:", 
            min(merged_data$InvoiceDate), 
            "to", 
            max(merged_data$InvoiceDate)))

# -------------------------------
# 4. Verify No Missing Cluster or Segment Assignments
# -------------------------------
# Since we're using inner_join, there should be no NAs in Cluster or Segment
missing_clusters <- merged_data %>%
  filter(is.na(Cluster) | is.na(Segment)) %>%
  distinct(CustomerID)

if(nrow(missing_clusters) > 0) {
  warning("There are customers without Cluster or Segment assignments. These will be excluded from the analysis.")
} else {
  print("All customers have Cluster and Segment assignments.")
}

# -------------------------------
# 5. Aggregate Data to Create Customer-Level Metrics
# -------------------------------
print("Aggregating Data to Create Customer-Level Metrics...")
customer_metrics <- merged_data %>%
  group_by(CustomerID, Cluster, Segment) %>%
  summarise(
    Recency = as.numeric(difftime(max(InvoiceDate), min(InvoiceDate), units = "days")),
    Frequency = n(),
    Monetary = sum(UnitPrice * Quantity, na.rm = TRUE),
    .groups = 'drop'  # Ensures that the grouping doesn't persist
  )

# Display first few rows to verify
print("Customer Metrics:")
print(head(customer_metrics))

