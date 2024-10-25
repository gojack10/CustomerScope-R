library(ggplot2)
library(scales)
library(dplyr)

# Read the data
data <- read.csv("data/customer_churn_risk.csv")

# Set default theme for all plots
theme_set(theme_minimal() + theme(
  panel.background = element_rect(fill = "white"),
  plot.background = element_rect(fill = "white")
))

# 1. Recency vs Churn Probability with improved annotations
p1 <- ggplot(data, aes(x = Recency, y = Churn_Probability)) +
  geom_point(alpha = 0.4, color = "darkblue") +
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Customer Churn Risk Analysis",
       subtitle = "Showing two distinct customer segments: Active (bottom) vs At-Risk (top)",
       x = "Days Since Last Purchase",
       y = "Probability of Customer Churning (0-1)") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(labels = comma,
                    breaks = seq(0, 400, 50)) +
  annotate("text", x = 200, y = 0.9, 
           label = "High-Risk Segment\n(~100% churn probability)", 
           color = "red", size = 4) +
  annotate("text", x = 200, y = 0.1, 
           label = "Active Customers\n(<20% churn probability)", 
           color = "darkblue", size = 4) +
  theme(
    plot.subtitle = element_text(size = 11, color = "grey40"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold")
  )

# Create custom transformations
y_trans <- scales::trans_new(
  name = "custom_y",
  transform = function(x) ifelse(x <= 2, x * 5, 10),
  inverse = function(x) ifelse(x <= 10, x/5, 2)
)

x_trans <- scales::trans_new(
  name = "custom_x",
  transform = function(x) ifelse(x <= 100, x, 100 + (x-100)/3),
  inverse = function(x) ifelse(x <= 100, x, (x-100)*3 + 100)
)

# 2. Improved visualization with custom scale
p2 <- ggplot() +
  geom_point(data = data[order(data$Churn_Probability),], 
             aes(x = Recency, 
                 y = Transaction_Frequency,
                 color = Churn_Probability),
             size = 2,
             alpha = 0.6) +
  scale_color_gradient2(
    low = "blue",
    mid = "yellow",
    high = "red",
    midpoint = 0.5
  ) +
  scale_y_continuous(
    trans = y_trans,
    breaks = c(0, 1, 2),
    limits = c(0, 2)
  ) +
  scale_x_continuous(
    trans = x_trans,
    breaks = c(0, 50, 100, 200, 300, 400),
    labels = comma
  ) +
  labs(title = "Customer Churn Risk Analysis",
       subtitle = "Higher risk customers shown in front",
       x = "Days Since Last Purchase",
       y = "Transaction Frequency",
       color = "Churn\nProbability") +
  theme(legend.position = "right")

# Save plots to files
ggsave("plots/recency_vs_churn.png", p1, width = 10, height = 6, dpi = 300)
ggsave("plots/churn_heatmap.png", p2, width = 12, height = 8, dpi = 300)
