library(ggplot2)
library(scales)

# Import data_loading.R
source("marketing/data_loading.R")


# -------------------------------
# 13. Data Visualization
# -------------------------------
print("Creating Data Visualizations...")

# a. Boxplot of Monetary Value by Cluster
ggplot(customer_metrics, aes(x = as.factor(Cluster), y = Monetary, fill = as.factor(Cluster))) +
  geom_boxplot() +
  theme_minimal(base_family = "sans") +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = NA)) +  # Set entire plot background to white
  labs(title = "Boxplot of Monetary Value by Cluster", x = "Customer Cluster", y = "Monetary Value") +
  scale_y_continuous(labels = scales::comma)
ggsave("plots/boxplot_monetary_by_cluster.png", bg = "white")  # Ensure saved plot has white background

# b. Scatter Plot of Frequency vs. Monetary Value
ggplot(customer_metrics, aes(x = Frequency, y = Monetary, color = as.factor(Cluster))) +
  geom_point(alpha = 0.6) +
  theme_minimal(base_family = "sans") +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Frequency vs. Monetary Value by Cluster", x = "Frequency", y = "Monetary Value") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma)
ggsave("plots/scatter_frequency_vs_monetary.png", bg = "white")

# c. Histogram of Recency
ggplot(customer_metrics, aes(x = Recency, fill = as.factor(Cluster))) +
  geom_histogram(binwidth = 10, alpha = 0.7, position = "dodge") +
  theme_minimal(base_family = "sans") +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Histogram of Recency by Cluster", x = "Recency (days)", y = "Count") +
  scale_x_continuous(labels = scales::comma)
ggsave("plots/histogram_recency_by_cluster.png", bg = "white")
