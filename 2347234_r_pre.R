
# Load the data
data <- read.csv("updated_file.csv")

# Descriptive statistics
summary(data)
# Check for missing or non-numeric values in the "IT" column
missing_values <- sum(is.na(data$IT) | !is.numeric(data$IT))

if (missing_values > 0) {
  # If there are missing or non-numeric values, print a message and handle them
  cat("Warning: Missing or non-numeric values found in the 'IT' column. Handling missing values.\n")
  
  # Replace missing or non-numeric values with a default value (e.g., 0)
  data$IT[!is.numeric(data$IT)] <- 0
}

# Scatter plot for Age and IT with color
plot(data$Age, data$IT, main = "Scatter Plot of Age and IT", xlab = "Age", ylab = "IT", col = "red", pch = 16)



# Scatter plot for Age with color
plot(data$Age, main = "Scatter Plot of Age", xlab = "Age", col = "green", pch = 16)

# Scatter plot for Age and Income with color
#plot(data$Age, data$Income, main = "Scatter Plot of Age and Income", xlab = "Age", ylab = "Income", col = "blue", pch = 16)

# Scatter plot for Age and IT with color
#plot(data$Age, data$IT, main = "Scatter Plot of Age and IT", xlab = "Age", ylab = "IT", col = "red", pch = 16)
