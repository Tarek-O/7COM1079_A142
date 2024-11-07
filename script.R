# Uncomment the line below to install the core libraries if this is the first time the script is being run
# install.packages("tidyverse")

# Load the core libraries
library(readr)
library(dplyr)

#Allocating variable "df" as a reference to the dataset DS301 
df<-read.csv("HRDataset_v14.csv")

#Created a subset of the original dataset to clean the data
df2<-subset(df,State == "MA")

# Extract the variables used for analysis (Performance Score, Salary) along with the Details of the Employee
EMPLOYEE_COLUMNS <- c("EmpID", "Employee_Name", "PerformanceScore", "Salary")
df3 <- df2[EMPLOYEE_COLUMNS]

#Visualize the frequency of Salaries based on the cleaned data which looks normal
### THERE IS AN ERROR IN HISTOGRAM FUNCTION###
hist(df2$Salary, format(df2$Salary, scientific = F), main = "Histogram of Salary Distribution", xlab = "Salaries")

#Visualized the data using a boxplot function with Salary as x-axis and Performance Score as y-axis
boxplot(df2$Salary ~ df2$PerformanceScore, xlab =
          "Perforamnce Score", ylab = "Salary", main = "Salary Based on Performance Score")

#Calculating the mean of all salaries
mean(df2$Salary)