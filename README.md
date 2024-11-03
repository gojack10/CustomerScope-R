# CustomerScope: RFM Analytics & Strategic Segmentation

A comprehensive e-commerce customer segmentation analysis using RFM (Recency, Frequency, Monetary) metrics and advanced clustering techniques. This project helps businesses understand and optimize their customer relationships through data-driven insights and actionable marketing strategies.

For detailed methodology and findings, please refer to my [research paper](ecommerce_customer_segmentation_tenbosch_2024.md).

## Features

### Analytics & Segmentation
- **RFM Analysis**
  - Recency: Time since last purchase
  - Frequency: Purchase count analysis
  - Monetary: Customer lifetime value tracking

- **Advanced Clustering**
  - K-means clustering with optimal cluster detection
  - Elbow method for cluster optimization
  - Silhouette analysis for cluster validation
  - Customer behavior pattern identification

### Interactive Dashboard
- Real-time customer metrics
- Segment distribution visualization
- Churn risk prediction
- Purchase pattern forecasting
- Customizable reporting

## Customer Segments

Our analysis identified four distinct customer segments:

| Segment | Percentage | Characteristics |
|---------|------------|-----------------|
| VIP Customers | 0.30% | High-value, frequent buyers with strong loyalty |
| Champions | 4.70% | Regular customers with consistent engagement |
| Potential Loyalists | 70.54% | Moderate engagement with growth potential |
| Average Customers | 24.46% | Occasional buyers with opportunity for engagement |

## How to Run

### Option 1: Run Directly from GitHub (Recommended)

1. **Install and load the Shiny package**:
   ```R
   install.packages("shiny")
   library(shiny)
   ```

2. **Run the dashboard**:
   ```R
   runGitHub("CustomerScope-R", "gojack10")
   ```

### Option 2: Run Locally

1. **Clone and setup**:
   ```bash
   git clone https://github.com/gojack10/CustomerScope-R
   cd CustomerScope-R
   ```

2. **Install dependencies in R**:
    ```R
    install.packages(c("tidyverse", "scales", "DT", "shiny", "shinydashboard", "plotly", "prophet", "cluster", 
    "factoextra", "caret", "randomForest", "readxl", "readr", "lubridate", "ggplot2"))
    ```

3. **Launch locally**:
   ```R
   shiny::runApp()
   ```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.