# Load necessary packages
library(ggplot2)
library(dplyr)
library(ggfortify)
library(forecast)

# Read the dataset
laus <- read.csv("laus.csv")

# Data cleaning
# Convert relevant columns to appropriate data types
laus$Year <- as.Date(as.character(laus$Year), format = "%Y")
laus$Date <- as.Date(laus$Date)

# Research Question 1: Linear Relationship Between Unemployment Rate and Labor Force
# Fit a linear regression model
model <- lm(UnemploymentRate ~ LaborForce, data = laus)
# Display model summary
summary(model)

# Create a scatter plot with a linear regression line
ggplot(laus, aes(x = LaborForce, y = UnemploymentRate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Linear Relationship Between Unemployment Rate and Labor Force",
       x = "Labor Force", y = "Unemployment Rate")


# Research Question 2: Analyzing Employment and Unemployment Discrepancy
laus$EmploymentDiscrepancy <- laus$LaborForce - laus$Employment
ggplot(laus, aes(x = Year, y = EmploymentDiscrepancy)) +
  geom_line() +
  labs(title = "Employment and Unemployment Discrepancy Over Time", x = "Year", y = "Employment Discrepancy")

# Research Question 3: Trends in Unemployment Rate by Area Type
mean_unemployment_by_type <- laus %>%
  group_by(Area.Type) %>%
  summarise(mean_unemployment = mean(UnemploymentRate, na.rm = TRUE))
ggplot(mean_unemployment_by_type, aes(x = Area.Type, y = mean_unemployment)) +
  geom_bar(stat = "identity") +
  labs(title = "Trends in Unemployment Rate by Area Type", x = "Area Type", y = "Mean Unemployment Rate")

# Research Question 4: Identify Outliers in Unemployment Rate Over the Years
outlier_threshold <- 1.5
outliers <- boxplot.stats(laus$UnemploymentRate)$out

ggplot(laus, aes(x = Year, y = UnemploymentRate)) +
  geom_point() +
  geom_point(data = filter(laus, UnemploymentRate %in% outliers),
             color = "red", size = 3) +
  labs(title = "Outliers in Unemployment Rate Over the Years",
       x = "Year", y = "Unemployment Rate")

