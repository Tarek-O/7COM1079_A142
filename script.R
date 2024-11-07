# Uncomment the line below to install the core libraries if this is the first time the script is being run
# install.packages("tidyverse")

# Load the core libraries
library(readr)
library(dplyr)

#Allocating variable "df" as a reference to the dataset DS301 
raw_data<-read.csv("HRDataset_v14.csv")

#Created a subset of the original dataset to clean the data
### Is there a purpose to this? Just for clarity are we only doing our research in the state of massachusetts? ###
massachusetts_employee_data<-subset(raw_data, State == "MA")

# Extract the variables used for analysis (Performance Score, Salary) along with the Details of the Employee
EMPLOYEE_COLUMNS <- c("EmpID", "Employee_Name")
FEATURE_COLUMNS <- c("PerformanceScore", "Salary")
FINAL_COLUMNS <- c(EMPLOYEE_COLUMNS, FEATURE_COLUMNS)
massachusetts_employee_data_projection <- massachusetts_employee_data[FINAL_COLUMNS]

# Visualize the frequency of Salaries based on the cleaned data which looks normal
hist(massachusetts_employee_data_projection$Salary, main = "Histogram of Salary Distribution", xlab = "Salaries", xaxt = "n")
axis(1, at = pretty(massachusetts_employee_data_projection$Salary), labels = format(pretty(massachusetts_employee_data_projection$Salary), scientific = FALSE))

#Visualized the data using a boxplot function with Salary as x-axis and Performance Score as y-axis
boxplot(massachusetts_employee_data_projection$Salary ~ massachusetts_employee_data_projection$PerformanceScore, xlab =
          "Perforamnce Score", ylab = "Salary", main = "Salary Based on Performance Score", las = 1)

#Calculating the mean of all salaries
mean(massachusetts_employee_data_projection$Salary)