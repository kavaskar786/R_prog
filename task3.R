# Task 1
employee <- list(EmployeeID = 01, Name = "likitha", Salary = 100000, Departments = c("Sales", "Marketing"))
print(employee)

# Task 2
annual_salary <- employee$Salary * 12
cat("Annual salary: $", annual_salary, "\n", sep = "")

# Task 3
employee$Name <- "aasha"
print(employee)

# Task 4
employee$Departments <- c(employee$Departments, "Human Resources")
print(employee)

# Task 5
employee2 <- list(EmployeeID = 02, Name = "lahari", Salary = 7500, Departments = c("Sales", "Finance"))
employee3 <- list(EmployeeID = 03, Name = "kalpana", Salary = 12500, Departments = c("Marketing", "Finance"))
organization <- list(Name = "christ", Employees = list(employee, employee2, employee3))
print(organization)

# Task 6
annual_salary <- organization$Employees[[2]]$Salary * 12
cat("Annual salary of the second employee: $", annual_salary, "\n", sep = "")

# Task 7
cat("Name of the organization: ", organization$Name, "\n", sep = "")

# Task 8
department_employees <- list()
for ( employee1 in organization$Employees) {
  for (department in employee$Departments) {
    if (department %in% names(department_employees)) {
      department_employees[[department]] <- c(department_employees[[department]], employee1)
    } else {
      department_employees[[department]] <- list(employee1)
    }
  }
}
print(department_employees)

# Task 9
print(employee)
print(employee2)
print(employee3)
print(organization)
print(department_employees)

