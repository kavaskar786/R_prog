# Task 1: Create a sample data frame with new values
employee_data <- data.frame(
  EmployeeID = 11:20,
  Name = c("Alice", "Bob", "Charlie", "David", "Eva", "Frank", "Grace", "Henry", "Ivy", "Jack"),
  Department = c("Finance", "HR", "IT", "Marketing", "R&D", "Finance", "IT", "Marketing", "R&D", "HR"),
  Salary = c(60000, 72000, 28500, 35200, 61900, 50200, 22300, 70100, 90900, 12400),
  JoiningDate = c("2018-06-20", "2017-09-14", "2022-02-10", "2019-05-05", "2021-08-15", "2018-11-18", "2020-04-22", "2016-07-15", "2018-12-28", "2017-08-04")
)

# Save the data frame to a CSV file
write.csv(employee_data, "employee_data_modified.csv", row.names = FALSE)
cat("Task 1: Sample data frame created and saved to 'employee_data_modified.csv'\n\n")

# Task 2: Load the modified dataset from "employee_data_modified.csv" into a data frame in R
employee_data <- read.csv("employee_data_modified.csv")
cat("Task 2: Data loaded from 'employee_data_modified.csv'\n\n")

# Task 3: Display the structure of the modified data frame
str(employee_data)
cat("Task 3: Displayed the structure of the data frame\n\n")

# Task 4: Calculate and add a new column named "Years of Service"
employee_data$JoiningDate <- as.Date(employee_data$JoiningDate)
employee_data$YearsOfService <- as.numeric(difftime(Sys.Date(), employee_data$JoiningDate, units = "days")) / 365.25
cat("Task 4: Calculated and added 'Years of Service' column\n\n")

# Task 5: Create a new data frame named "Experienced Employees"
experienced_employees <- subset(employee_data, YearsOfService >= 4)
cat("Task 5: Created 'Experienced Employees' data frame\n\n")

# Task 6: Calculate and print the average salary of employees in each department
avg_salary_by_dept <- aggregate(Salary ~ Department, data = employee_data, FUN = mean)
print(avg_salary_by_dept)
cat("Task 6: Calculated and printed average salary by department\n\n")

# Task 7: Determine the highest and lowest salaries and identify the employees with these salaries
highest_salary <- max(employee_data$Salary)
lowest_salary <- min(employee_data$Salary)

employees_with_highest_salary <- subset(employee_data, Salary == highest_salary)
employees_with_lowest_salary <- subset(employee_data, Salary == lowest_salary)

cat("Task 7: Determined highest and lowest salaries\n")
cat("Highest Salary: $", highest_salary, "\n")
cat("Employees with Highest Salary:\n")
print(employees_with_highest_salary)
cat("Lowest Salary: $", lowest_salary, "\n")
cat("Employees with Lowest Salary:\n")
print(employees_with_lowest_salary)
cat("\n")

# Task 8: Create a bar plot to visualize the number of employees in each department
barplot(table(employee_data$Department), col = "green", main = "Number of Employees in Each Department")
cat("Task 8: Created a bar plot\n\n")

# Task 9: Generate a scatter plot to explore the relationship between years of service and salary
plot(employee_data$YearsOfService, employee_data$Salary, xlab = "Years of Service", ylab = "Salary", main = "Scatter Plot of Years of Service vs. Salary", col = "purple")
cat("Task 9: Generated a scatter plot\n\n")

# Task 10: Save the "Experienced Employees" data frame as a CSV file named "experienced_employees.csv"
write.csv(experienced_employees, "experienced_employees.csv", row.names = FALSE)
cat("Task 10: Saved 'Experienced Employees' data frame to 'experienced_employees.csv'\n\n")
