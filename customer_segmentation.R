# Load required libraries
library(tidyverse)  # For data manipulation and visualization
library(cluster)    # For clustering algorithms
library(factoextra) # For clustering visualization
library(readxl)     # For reading Excel files
library(lubridate)  # For date manipulation

# Read the Online Retail Dataset
retail_data <- read_excel("data/online-retail.xlsx")

# Basic data exploration
glimpse(retail_data)

# Data cleaning and preparation
clean_retail_data <- retail_data %>%
  # Remove rows with missing values
  na.omit() %>%
  # Remove rows with negative quantities or prices
  filter(Quantity > 0, UnitPrice > 0) %>%
  # Convert InvoiceDate to proper datetime format
  mutate(InvoiceDate = as.Date(InvoiceDate))

# Get the latest date in the dataset to calculate recency
max_date <- max(clean_retail_data$InvoiceDate)

# Create RFM features
rfm_data <- clean_retail_data %>%
  group_by(CustomerID) %>%
  summarise(
    # Recency: days since last purchase
    Recency = as.numeric(difftime(max_date, max(InvoiceDate), units = "days")),
    # Frequency: number of purchases
    Frequency = n_distinct(InvoiceNo),
    # Monetary: total money spent
    Monetary = sum(Quantity * UnitPrice)
  ) %>%
  # Remove any rows where CustomerID is NA
  filter(!is.na(CustomerID))

# Scale the features
rfm_scaled <- scale(rfm_data[, c("Recency", "Frequency", "Monetary")])

# Basic summary of RFM metrics
summary(rfm_data)

# Create a simple visualization of the distributions
rfm_plots <- rfm_data %>%
  gather(key = "Metric", value = "Value", -CustomerID) %>%
  ggplot(aes(x = Value)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  facet_wrap(~Metric, scales = "free") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  labs(title = "Distribution of RFM Metrics",
       x = "Value",
       y = "Count")

# Display the plot
print(rfm_plots)

# Save the plot
ggsave("plots/rfm_distributions.png", rfm_plots, width = 12, height = 6)

# Elbow Method Analysis
set.seed(123) # For reproducibility
wss <- numeric(10)
for (k in 1:10) {
  km <- kmeans(rfm_scaled, centers = k, nstart = 25)
  wss[k] <- km$tot.withinss
}

# Plot Elbow Method
elbow_plot <- ggplot(data.frame(k = 1:10, wss = wss), aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  labs(title = "Elbow Method for Optimal k",
       x = "Number of Clusters (k)",
       y = "Within-cluster Sum of Squares")

# Save elbow plot
ggsave("plots/elbow_plot.png", elbow_plot, width = 8, height = 6)

# Silhouette Analysis
silhouette_scores <- numeric(9)
for (k in 2:10) {
  km <- kmeans(rfm_scaled, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(rfm_scaled))
  silhouette_scores[k-1] <- mean(ss[, 3])
}

# Plot Silhouette Scores
silhouette_plot <- ggplot(data.frame(k = 2:10, score = silhouette_scores), 
                         aes(x = k, y = score)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  labs(title = "Silhouette Analysis for Optimal k",
       x = "Number of Clusters (k)",
       y = "Average Silhouette Score")

# Save silhouette plot
ggsave("plots/silhouette_plot.png", silhouette_plot, width = 8, height = 6)

# Perform k-means clustering with optimal k (let's say k=4 for now)
optimal_k <- 4  # We'll adjust this based on the elbow and silhouette analysis
final_clusters <- kmeans(rfm_scaled, centers = optimal_k, nstart = 25)

# Add cluster assignments to the original RFM data
rfm_clustered <- rfm_data %>%
  mutate(Cluster = factor(final_clusters$cluster))

# Calculate cluster centers
cluster_centers <- data.frame(
  Cluster = factor(1:optimal_k),
  bind_rows(lapply(1:optimal_k, function(i) {
    rfm_clustered %>%
      filter(Cluster == i) %>%
      summarise(
        Recency = mean(Recency),
        Frequency = mean(Frequency),
        Monetary = mean(Monetary),
        Size = n()
      )
  }))
)

# Create a summary visualization of clusters
cluster_summary_plot <- rfm_clustered %>%
  gather(key = "Metric", value = "Value", Recency, Frequency, Monetary) %>%
  ggplot(aes(x = Cluster, y = Value, fill = Cluster)) +
  geom_boxplot() +
  facet_wrap(~Metric, scales = "free_y") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white")) +
  labs(title = "Customer Segments Characteristics",
       x = "Cluster",
       y = "Value")

# Save cluster summary plot
ggsave("plots/cluster_summary.png", cluster_summary_plot, width = 12, height = 6)

# Print cluster centers
print("Cluster Centers:")
print(cluster_centers)

# Calculate percentile ranks for easier interpretation
rfm_clustered <- rfm_clustered %>%
  mutate(
    R_Score = ntile(desc(Recency), 5),    # 5 = most recent
    F_Score = ntile(Frequency, 5),        # 5 = highest frequency
    M_Score = ntile(Monetary, 5)          # 5 = highest monetary value
  )

# Create cluster profiles
cluster_profiles <- rfm_clustered %>%
  group_by(Cluster) %>%
  summarise(
    Size = n(),
    Size_Percentage = n() / nrow(rfm_clustered) * 100,
    Avg_Recency = mean(Recency),
    Avg_Frequency = mean(Frequency),
    Avg_Monetary = mean(Monetary),
    Avg_R_Score = mean(R_Score),
    Avg_F_Score = mean(F_Score),
    Avg_M_Score = mean(M_Score)
  ) %>%
  arrange(desc(Size))

# Print detailed cluster profiles
print("Detailed Cluster Profiles:")
print(cluster_profiles)

# Create segment labels based on RFM scores
cluster_labels <- cluster_profiles %>%
  mutate(
    Segment = case_when(
      Avg_R_Score >= 4 & Avg_F_Score >= 4 & Avg_M_Score >= 4 ~ "Champions",
      Avg_R_Score >= 4 & Avg_F_Score >= 3 & Avg_M_Score >= 3 ~ "Loyal Customers",
      Avg_R_Score >= 3 & Avg_F_Score >= 1 & Avg_M_Score >= 2 ~ "Potential Loyalists",
      Avg_R_Score < 2 & Avg_F_Score < 2 & Avg_M_Score < 2 ~ "Lost Customers",
      TRUE ~ "Average Customers"
    )
  )

# Print segment labels
print("\nCluster Segments:")
print(select(cluster_labels, Cluster, Segment, Size_Percentage))

# Function to save cluster profiles to a CSV file
save_cluster_profiles <- function(cluster_profiles, file_path) {
  write.csv(cluster_profiles, file = file_path, row.names = FALSE)
}

# Function to save customer segments to a CSV file
save_customer_segments <- function(rfm_clustered, cluster_labels, file_path) {
  # Join the clustered data with the cluster labels
  customer_segments <- rfm_clustered %>%
    left_join(cluster_labels, by = "Cluster") %>%
    select(CustomerID, Cluster, Segment)
  
  # Write to CSV
  write.csv(customer_segments, file = file_path, row.names = FALSE)
}

# Save the cluster profiles to a CSV file in the /data/ folder
save_cluster_profiles(cluster_profiles, "data/cluster_profiles.csv")

# Save the customer segments to a CSV file in the /data/ folder
save_customer_segments(rfm_clustered, cluster_labels, "data/customer_segments.csv")
