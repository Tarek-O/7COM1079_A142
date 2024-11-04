install.packages("tidyverse")
library(readr)

df<-read.csv("HRDataset_v14.csv")

boxplot(df$Salary ~ df$PerformanceScore, xlab =
          "Perforamnce Score", ylab = "Salary", main = "Salary on Performance Score")

mean(df$Salary)