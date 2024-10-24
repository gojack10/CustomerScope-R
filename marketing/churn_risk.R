# Load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)

# Source the data loading script
source("marketing/data_loading.R")

# Create features for churn prediction from customer_metrics
churn_data <- customer_metrics %>%
  mutate(
    # Create churn flag based on multiple factors:
    # 1. High recency (> 180 days instead of 90)
    # 2. Low frequency relative to time period
    # 3. Declining monetary value
    Churn = factor(ifelse(
      Recency > 180 & 
      (Frequency / Recency) < 0.1 & # Less than 1 purchase per 10 days
      Monetary < mean(Monetary), 
      "Yes", "No")),
    
    # Normalize monetary value
    Monetary_Normalized = scale(Monetary),
    
    # Create additional features
    Average_Transaction = Monetary / Frequency,
    Transaction_Frequency = Frequency / Recency,
    
    # Add engagement score (0-1)
    Engagement_Score = (
      scale(Frequency) + 
      scale(Monetary) + 
      scale(-Recency)  # Negative because lower recency is better
    ) / 3
  ) %>%
  # Remove any infinite values
  filter(is.finite(Transaction_Frequency))

# Data preprocessing
# Handle missing values
churn_data <- churn_data %>% drop_na()

# Encode categorical variables as factors
churn_data <- churn_data %>%
  mutate_if(is.character, as.factor)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(churn_data$Churn, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
trainData <- churn_data[trainIndex, ]
testData <- churn_data[-trainIndex, ]

# Train a Random Forest model
rf_model <- randomForest(Churn ~ ., data = trainData, importance = TRUE)

# Instead of predicting only on test data, let's predict on all customers
all_predictions <- predict(rf_model, churn_data, type = "prob")[,2]

# Create a comprehensive customer risk dataframe
customer_risk <- churn_data %>%
  mutate(
    Churn_Probability = all_predictions
  ) %>%
  select(CustomerID, Cluster, Segment,
         Recency, Frequency, Monetary,
         Monetary_Normalized, Average_Transaction,
         Transaction_Frequency, Churn_Probability) %>%
  # Convert any list or matrix columns to numeric
  mutate(across(where(is.numeric), as.numeric)) %>%
  # Sort by churn probability (highest risk first)
  arrange(desc(Churn_Probability))

# Export all customers with their risk scores
write_csv(customer_risk, "data/customer_churn_risk.csv")

# Optional: Save the trained model for future use
saveRDS(rf_model, "rf_churn_model.rds")

# Print summary statistics
print("Churn Risk Summary:")
print(summary(customer_risk$Churn_Probability))
print(paste("Number of high-risk customers (>70% probability):",
            sum(customer_risk$Churn_Probability > 0.7)))
