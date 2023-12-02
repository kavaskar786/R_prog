
# Install and load necessary libraries
#install.packages("tidyverse")
library(tidyverse)

# Load the data
data <- read.csv("updated_file.csv")

# Descriptive statistics
summary(data)

str(data)

# Preprocessing: Handle missing or non-numeric values in the "IT" column
missing_values <- sum(is.na(data$IT) | !is.numeric(data$IT))

if (missing_values > 0) {
  # If there are missing or non-numeric values, print a message and handle them
  cat("Warning: Missing or non-numeric values found in the 'IT' column. Handling missing values.\n")
  
  # Replace missing or non-numeric values with a default value (e.g., 0)
  data$IT[!is.numeric(data$IT)] <- 0
}

# Standardize numerical variables (Age and Income)
data_scaled <- scale(data[, c("Age")])

# Histogram of standardized Age
par(mfrow = c(1, 1))
hist(data$Age, main = "Histogram of Standardized Age", xlab = "Age", col = "lightblue", border = "black")

# Bar chart for Gender distribution
par(mfrow = c(1, 1))  # Set the layout to one plot
barplot(table(data$Gender), main = "Gender Distribution", xlab = "Gender", ylab = "Count", col = "skyblue")

# Boxplot for standardized Age by Gender
par(mfrow = c(1, 1))  # Set the layout to one plot
boxplot(data$Age ~ data$Gender, main = "Boxplot of Standardized Age by Gender", xlab = "Gender", ylab = "Standardized Age", col = c("lightgreen", "lightcoral"))

# Scatter plot for Age
#plot(data$Age, main = "Scatter Plot of Age", xlab = "Age", col = "blue")

# Remove outliers from standardized Age
data_no_outliers <- data[!boxplot.stats(data$Age)$out, ]

# Outcome: Explore standardized Age distribution, understand the Gender distribution, examine Age differences by Gender, visualize Age and Income relationship after standardization, handle missing or non-numeric values, and remove outliers.
