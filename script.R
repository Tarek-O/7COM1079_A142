# Uncomment the line below to install the core libraries if this is the first time the script is being run
# install.packages("tidyverse")

# Load the core libraries
library(readr)
library(dplyr)

#Allocating variable "df" as a reference to the dataset DS301
raw_data<-read.csv("HRDataset_v14.csv")

# Constants
EMPLOYEE_COLUMNS <- c("EmpID", "Employee_Name")
FEATURE_COLUMNS <- c("MaritalDesc", "Salary")
FINAL_COLUMNS <- c(EMPLOYEE_COLUMNS, FEATURE_COLUMNS)
DEPENDENT_VARIABLE <- "MaritalDesc"
INDEPENDENT_VARIABLE <- "Salary"

#Created a subset of the original dataset to clean the data
massachusetts_employee_data<-subset(raw_data, MaritalDesc %in% c("Married", "Single"))

# group count of employees by position ordered by count
grouped_data <- massachusetts_employee_data %>%
  group_by(MaritalDesc) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Mean salary of employees by Marital Status
mean_salary_by_marital_status <- massachusetts_employee_data %>%
  group_by(MaritalDesc) %>%
  summarise(mean_salary = mean(Salary))

# Extract the variables used for analysis (Performance Score, Salary) along with the Details of the Employee
massachusetts_employee_data_projection <- massachusetts_employee_data[FINAL_COLUMNS]

# Visualize the frequency of Salaries based on the cleaned data which looks normal
hist(massachusetts_employee_data_projection[[INDEPENDENT_VARIABLE]],
     main = "Histogram of Salary Distribution",
     xlab = "Salaries",
     xaxt = "n",
     prob = TRUE)
# Format the x-axis
axis(1, at = pretty(massachusetts_employee_data_projection[[INDEPENDENT_VARIABLE]]),
     labels = format(pretty(massachusetts_employee_data_projection[[INDEPENDENT_VARIABLE]]), scientific = FALSE))
# Format the y-axis
axis(2, at = pretty(density(massachusetts_employee_data_projection[[INDEPENDENT_VARIABLE]])$y),
     labels = format(pretty(density(massachusetts_employee_data_projection[[INDEPENDENT_VARIABLE]])$y), scientific = FALSE))
# Add a bell curve
curve(dnorm(x, mean = mean(massachusetts_employee_data_projection[[INDEPENDENT_VARIABLE]]),
            sd = sd(massachusetts_employee_data_projection[[INDEPENDENT_VARIABLE]])),
      add = TRUE, col = "blue", lwd = 2)

# wilcox.test(massachusetts_employee_data_projection[[INDEPENDENT_VARIABLE]] ~ massachusetts_employee_data_projection[[DEPENDENT_VARIABLE]])

#Visualized the data using a boxplot function with Salary as x-axis and Performance Score as y-axis
boxplot(massachusetts_employee_data_projection[[INDEPENDENT_VARIABLE]] ~ massachusetts_employee_data_projection[[DEPENDENT_VARIABLE]],
        xlab = "Perforamnce Score",
        ylab = "Salary",
        main = "Salary Based on Performance Score",
        las = 1,
        col = c("lightgreen", "lightblue", "orange", "red"))
abline(h = mean(massachusetts_employee_data_projection[[INDEPENDENT_VARIABLE]]), col = "red", lwd = 2)
#Calculating the mean of all salaries
mean_salary <- mean(massachusetts_employee_data_projection[[INDEPENDENT_VARIABLE]])

# Add a legend to the boxplot to show the mean salary
legend("topright", legend = paste("Mean Salary: ", round(mean_salary, 2)), fill = "red")