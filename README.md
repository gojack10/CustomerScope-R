# CustomerScope: RFM Analytics & Strategic Segmentation

A comprehensive e-commerce customer segmentation analysis using RFM (Recency, Frequency, Monetary) metrics and advanced clustering techniques. This project analyzes customer purchasing behavior to identify distinct customer segments and provide actionable marketing insights.

For detailed methodology and findings, please refer to my [research paper](ecommerce_customer_segmentation_tenbosch_2024.md).

## Key Features

- **RFM Analysis**: Sophisticated customer behavior analysis using:
  - Recency (days since last purchase)
  - Frequency (number of purchases)
  - Monetary value (total spend)

- **Advanced Clustering**: Implementation of K-means clustering with optimal cluster determination using:
  - Elbow Method
  - Silhouette Analysis

- **Interactive Dashboard**: Comprehensive visualization and analysis tools including:
  - Customer segment profiles
  - Churn risk assessment
  - Time series forecasting
  - Real-time metrics

## Key Findings

Our analysis revealed four distinct customer segments:

1. VIP Customers (0.30%): High-value, frequent purchasers
2. Champions (4.70%): Consistent, engaged customers
3. Potential Loyalists (70.54%): Moderate engagement
4. Average Customers (24.46%): Lower engagement

## Technical Stack

- **R**: Primary programming language
- **Libraries**:
  - `dplyr`: Data manipulation
  - `ggplot2`: Data visualization
  - `cluster`: Clustering algorithms
  - `prophet`: Time series forecasting
  - `shiny`: Interactive dashboard

## Getting Started

1. Clone the repository
2. Install required R packages
3. Run the Shiny dashboard

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details