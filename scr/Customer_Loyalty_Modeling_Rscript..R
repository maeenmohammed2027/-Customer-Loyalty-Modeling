# -----------------------------------------------
# LSE Data Analytics | Module 6 Final Assignment
# Author: [Maenn Mohammed]
# Load libraries
library(tidyverse)
library(skimr)
library(DataExplorer)
library(dplyr)
library(forecast)
library(psych)
library(scatterplot3d)
library(psych)
library(moments)
# ------------------------------------------------------------------
# LOAD AND CLEAN DATA
# ------------------------------------------------------------------
game_review<-read.csv("turtle_reviews.csv")
colnames(game_review)
# drop  the language  and platform  columns
clean_df <- subset(game_review, select = -c(language, platform))
#rename columns remuneration and ‘spending_score
clean_df <- clean_df %>%
  rename(
    remuneration = `remuneration..k..`,
    spending_score = `spending_score..1.100.`
  )
#Load and explore the data:
head(clean_df)
summary(clean_df)
# check if there is any missing values 
skim(clean_df) # "skim() shows no missing data except 110 NAs in age_group
sum(is.na(clean_df))# No missing values detected in the dataset
# ------------------------------------------------------------------
# Exploratory Data Analysis (EDA)
# ------------------------------------------------------------------
# Check the distribution and normality of loyalty points. 
# 3. Histogram: Loyalty Points Distribution
ggplot(clean_df, aes(x = loyalty_points)) +
  geom_histogram(binwidth = 250, color = "black", fill = "white") +
  labs(title = "Loyalty Points Distribution", y = "Number of Customers") +
  theme_classic()
kurtosi(clean_df$loyalty_points)
skewness(clean_df$loyalty_points)
jarque.test(clean_df$loyalty_points)

# 1. Boxplot: Spending Score vs Loyalty Points
ggplot(clean_df, aes(x = factor(cut(spending_score, breaks = 4)), y = loyalty_points)) +
  geom_boxplot() +
  labs(title = "Spending Score vs Loyalty Points (Boxplot)",x="Spending  Sore",y="Loyalty Points),",
       x="Spending Score",y="loyalty_points")

# 2. Boxplot: Remuneration vs Loyalty Points
ggplot(clean_df, aes(x = factor(cut(remuneration, breaks = 4)), y = loyalty_points)) +
  geom_boxplot() +
  labs(title = "Remuneration vs Loyalty Points (Boxplot)",x="remuneration",y="Loyalty Points")

ggplot(clean_df, aes(x = remuneration, y = spending_score, color = loyalty_points)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_c() +
  labs(title = "Remuneration vs Spending Score Colored by Loyalty Points",
       x = "Remuneration (k£)", y = "Spending Score") +
  theme_minimal()

# 4. Bar chart: Loyalty by Education
df_edu_avg <- clean_df %>%
  group_by(education) %>%
  summarise(avg_loyalty = mean(loyalty_points), .groups = 'drop')

ggplot(df_edu_avg, aes(x = education, y = avg_loyalty)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average Loyalty Points by Education Level",
       x = "Education", y = "Avg Loyalty Points") +
  theme_classic()

# 5. Scatterplot with gender facet
ggplot(clean_df, aes(x = spending_score, y = loyalty_points)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  facet_grid(~gender) +
  labs(title = "Spending Score vs Loyalty Points by Gender") +
  geom_smooth(method = lm, se = FALSE) +
  theme_classic()

# 6. Scatterplot with gender & age group
clean_df <- clean_df %>%
  mutate(age_group = cut(age, breaks = c(18, 30, 40, 50, 60, 100), 
                         labels = c("18–30", "31–40", "41–50", "51–60", "60+")))

ggplot(clean_df, aes(x = spending_score, y = loyalty_points, color = age_group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_grid(~gender) +
  labs(title = "Spending vs Loyalty by Gender & Age Group",
       x = "Spending Score", y = "Loyalty Points")

# ---- Summary of EDA Findings ----
# - Loyalty Points Distribution is right-skewed, with most customers earning moderate points.
# - Spending Score and Remuneration show positive associations with Loyalty Points.
# - Customers with Basic education earn the highest average loyalty points.
# - Loyalty trends appear consistent across genders, with similar upward patterns.
# - Younger age groups (18–30) might respond more strongly to loyalty programs.

# Prepare regression dataset
df_regression <- clean_df %>%
  select(age, remuneration, spending_score, loyalty_points, product)
head(df_regression)

# Correlation Analysis
corPlot(df_regression)
# ➤ The correlation plot reveals strong positive relationships between 
# remuneration, spending_score, and loyalty_points.
# ➤ These variables are selected as predictors for the regression model.

# Build multiple linear regression model
model01 <- lm(loyalty_points ~ remuneration + spending_score, data = df_regression)
summary(model01)

# Build simpler model using only spending_score
model02 <- lm(loyalty_points ~ spending_score, data = df_regression)
summary(model02)
# ➤ The simpler model has lower explanatory power (Adj. R² ≈ 0.45),
# confirming that remuneration adds significant predictive value.

# 3D Visualization of Actual Data
scatterplot3d(
  x = df_regression$remuneration,
  y = df_regression$spending_score,
  z = df_regression$loyalty_points,
  xlab = "Remuneration",
  ylab = "Spending Score",
  zlab = "Loyalty Points",
  main = "Observed Data: 3D Scatter Plot"
)

# Create new customer profiles to test the model
loyalty_test <- data.frame(
  customer = c("A", "B", "C"),
  remuneration = c(10, 12, 14),
  spending_score = c(20, 60, 80)
)

# Predict loyalty points with confidence intervals
loyalt_predict <- predict(model01, newdata = loyalty_test, interval = "confidence")
loyalty_test$predicted_loyalty_points <- loyalt_predict[, "fit"]
loyalt_predict
# ➤ Predictions align with higher remuneration and spending scores,
# supporting the model's practical value.

# Visualize predictions in 3D
plot3d <- scatterplot3d(
  x = loyalty_test$remuneration,
  y = loyalty_test$spending_score,
  z = loyalty_test$predicted_loyalty_points,
  xlab = "Remuneration",
  ylab = "Spending Score",
  zlab = "Predicted Loyalty Points",
  main = "Customer Loyalty Prediction",
  color = "steelblue",
  pch = 16
)

# Label test points A, B, C
coords <- plot3d$xyz.convert(
  loyalty_test$remuneration,
  loyalty_test$spending_score,
  loyalty_test$predicted_loyalty_points
)
text(
  x = coords$x,
  y = coords$y,
  labels = loyalty_test$customer,
  pos = 4,
  cex = 0.9
)

# Save plot image (if needed)
dev.off()

# 📌 Business Insights Summary

# This analysis evaluated whether customer loyalty points could be predicted using a Multiple Linear Regression (MLR) model. 
# Correlation analysis identified remuneration and spending_score as strong predictors. 
# The full model achieved an adjusted R² of approximately 0.65, indicating good predictive power.

# Predictions for new fictional customers aligned with expected behavior—those with higher income and spending showed higher loyalty scores.
# A simpler model using only spending_score performed significantly worse, confirming that both predictors add value.

# The loyalty programme appears to reward high-value customers effectively. 
# To improve inclusiveness and engagement, the model could be expanded to include non-monetary factors such as product category, platform use, or customer tenure. 
# Future iterations using advanced models (e.g., random forests) could further improve accuracy.

# Marketing strategies should focus on high-spending customers with mid-to-high remuneration, as these segments show the strongest loyalty potential.


# ----------------------------------------------------------------------
#📊 R Analysis: Loyalty Points Prediction
#Summary

#This R-based analysis explored customer loyalty patterns and built predictive models using Multiple Linear Regression (MLR).

#Data Preparation: Cleaned dataset by removing irrelevant variables and renaming columns for clarity.

# Exploratory Data Analysis (EDA):
  
#Loyalty points are right-skewed, with most customers earning moderate points.

#Both spending score and remuneration show strong positive relationships with loyalty points.

# Customers with Basic education level recorded the highest average loyalty points.

# Gender differences in loyalty trends are minimal, but younger age groups (18–30) show slightly stronger responses to loyalty programs.

## Modeling:
  
#The full regression model using both remuneration and spending score achieved an adjusted R² ≈ 0.65, indicating good predictive power.

# A simpler model using only spending score performed significantly worse (Adj. R² ≈ 0.45), confirming that both predictors add value.

# Predictions for fictional customers aligned with expectations — higher income and spending led to higher predicted loyalty.

## Business Recommendations

# Target High-Value Segments: Focus marketing and loyalty program incentives on customers with mid-to-high remuneration and high spending scores, as they demonstrate the strongest loyalty potential.

# Improve Inclusiveness: Consider enhancing loyalty rewards for lower-income but engaged customers to broaden participation.

# Expand Model Variables: Incorporate additional predictors such as product category, customer tenure, and purchase frequency to improve prediction accuracy.

# Monitor and Iterate: Periodically re-evaluate the model to ensure it reflects evolving customer behaviors, and test advanced models (e.g., Random Forest) for potential performance gains.
