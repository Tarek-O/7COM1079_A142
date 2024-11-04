install.packages("tidyverse")
library(readr)

#Allocating variable "df" as a reference to the dataset DS301 
df<-read.csv("HRDataset_v14.csv")

#Created a subset of the original dataset to clean the data
df2<-subset(df,Salary<100000)

#Visualize the frequency of Salaries based on the cleaned data which looks normal
hist(df2$Salary, main = "Histogram of Salary Distribution", xlab = "Salaries")

#Visualized the data using a boxplot function with Salary as x-axis and Performance Score as y-axis
boxplot(df2$Salary ~ df2$PerformanceScore, xlab =
          "Perforamnce Score", ylab = "Salary", main = "Salary on Performance Score")

#Calculating the mean of all salaries
mean(df2$Salary)