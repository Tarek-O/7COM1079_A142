install.packages("tidyverse")
library(readr)

#Allocating variable "df" as a reference to the dataset DS301 
df<-read.csv("HRDataset_v14.csv")

#Visualized the data using a boxplot function with Salary as x-axis and Performance Score as y-axis
boxplot(df$Salary ~ df$PerformanceScore, xlab =
          "Perforamnce Score", ylab = "Salary", main = "Salary on Performance Score")

#Calculating the mean of all salaries
mean(df$Salary)