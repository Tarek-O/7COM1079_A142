# Uncomment the line below to install the core libraries if this is the first time the script is being run
# install.packages("tidyverse")

# Load the core libraries
library(readr)
library(dplyr)
library(ggplot2)

#Allocating variable "df" as a reference to the dataset DS301
raw_data<-read.csv("HRDataset_v14.csv")

# Constants
EMPLOYEE_COLUMNS <- c("EmpID", "Employee_Name", "Department")
FEATURE_COLUMNS <- c("MaritalDesc", "Salary")
FINAL_COLUMNS <- c(EMPLOYEE_COLUMNS, FEATURE_COLUMNS)
DEPENDENT_VARIABLE <- "MaritalDesc"
INDEPENDENT_VARIABLE <- "Salary"

employee_data_projection <- raw_data[FINAL_COLUMNS]

# Parse all the string data in the dataset so that there are no leading or trailing whitespaces and
for (i in seq_len(ncol(employee_data_projection))) {
  if (is.character(employee_data_projection[[i]])) {
    employee_data_projection[[i]] <- trimws(employee_data_projection[[i]], which = "both")
  }
}


subset_bool <- employee_data_projection$Department == "Production" & employee_data_projection$MaritalDesc %in% c("Married", "Single")

#Created a subset of the original dataset to clean the data
employee_data<-subset(employee_data_projection, subset_bool)


# group count of employees by position ordered by count
grouped_data <- employee_data %>%
  group_by(MaritalDesc) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Mean salary of employees by Marital Status
mean_salary_by_marital_status <- employee_data %>%
  group_by(MaritalDesc) %>%
  summarise(mean_salary = mean(Salary))

# Extract the variables used for analysis (MartialDesc, Salary) along with the Details of the Employee

# Visualize the frequency of Salaries based on the cleaned data which looks normal
hist(employee_data[[INDEPENDENT_VARIABLE]],
     main = "Histogram of Salary Distribution ($)",
     xlab = "Salaries ($)",
     xaxt = "n",
     ylim = c(0, 50),
     las = 1,
     freq = TRUE,
     scientific = FALSE
)
# Format the x-axis
axis(1, at = pretty(employee_data[[INDEPENDENT_VARIABLE]]),
     labels = format(pretty(employee_data[[INDEPENDENT_VARIABLE]]), scientific = FALSE))
# Add a bell curve
curve(dnorm(x, mean = mean(employee_data[[INDEPENDENT_VARIABLE]]),
            sd = sd(employee_data[[INDEPENDENT_VARIABLE]])) * length(employee_data[[INDEPENDENT_VARIABLE]]) * diff(hist(employee_data[[INDEPENDENT_VARIABLE]], plot = FALSE)$breaks)[1],
      add = TRUE, col = "blue", lwd = 2)

# export to png
dev.copy(png, "salary_histogram.png")
dev.off()

shapiro.test(employee_data[[INDEPENDENT_VARIABLE]])

wilcox.test(employee_data[[INDEPENDENT_VARIABLE]] ~ employee_data[[DEPENDENT_VARIABLE]])

# Visualized the data using a boxplot function with Salary as x-axis and Marital Status as y-axis
boxplot <- boxplot(employee_data[[INDEPENDENT_VARIABLE]] ~ employee_data[[DEPENDENT_VARIABLE]],
        xlab = "Marital Status",
        ylab = "Salary ($)",
        main = "Salary Based on Marital Status",
        ylim = c(0, 180000),
        las = 1,
        col = c("lightgreen", "lightblue"))

abline(h = mean(employee_data[[INDEPENDENT_VARIABLE]]), col = "red", lwd = 2)


mean_salary <- mean(employee_data[[INDEPENDENT_VARIABLE]])


# ggplot(employee_data, aes(x = MaritalDesc, y = Salary)) +
#   geom_boxplot(fill = "lightblue") +
#   geom_hline(yintercept = mean_salary, color = "red", linetype = "dashed") +
#   labs(title = "Salary Based on Marital Description", x = "Marital Description", y = "Salary ($)") +
#   scale_fill_discrete(name = "Groups") +
#   scale_color_manual(name = "Statistics",      # Add mean to a separate legend
#                      values = c("Overall Mean" = "red")) +
#   theme(legend.title = element_text(face = "bold"))


# ggplot(data, aes(x = group, y = value)) +
#   geom_boxplot(aes(fill = group), alpha = 0.6) +  # Boxplot with group fill
#   geom_hline(aes(yintercept = overall_mean, color = "Overall Mean"),
#              linetype = "dashed", size = 1) +  # Add the mean line
#   scale_fill_discrete(name = "Groups") +       # Rename fill legend
#   scale_color_manual(name = "Statistics",      # Add mean to a separate legend
#                      values = c("Overall Mean" = "red")) +
#   labs(title = "Boxplot with Overall Me")


#Calculating the mean of all salaries

# Add a legend to the boxplot to show the mean salary
legend("topright", legend = paste("Mean Salary: ", round(mean_salary, 2)), fill = "red")

# export to png
dev.copy(png, "salary_boxplot.png")
dev.off()