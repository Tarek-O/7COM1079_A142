# Uncomment the line below to install the core libraries if this is the first time the script is being run
# install.packages("tidyverse")

# Load the core libraries
library(readr)
library(dplyr)
library(ggplot2)

#Allocating variable "raw_data" as a reference to the dataset DS301
raw_data<-read.csv("HRDataset_v14.csv")

# Constants
EMPLOYEE_COLUMNS <- c("EmpID", "Employee_Name", "Department")
FEATURE_COLUMNS <- c("MaritalDesc", "Salary")
FINAL_COLUMNS <- c(EMPLOYEE_COLUMNS, FEATURE_COLUMNS)
DEPENDENT_VARIABLE <- "MaritalDesc"
INDEPENDENT_VARIABLE <- "Salary"

employee_data_projection <- raw_data[FINAL_COLUMNS]

# Parse all the string data in the dataset so that there are no leading or trailing whitespaces and convert to lowercase
for (i in seq_len(ncol(employee_data_projection))) {
  if (!is.character(employee_data_projection[[i]])) next
  employee_data_projection[[i]] <- tolower(trimws(employee_data_projection[[i]], which = "both"))
}


subset_bool <- employee_data_projection$Department == "production" & employee_data_projection$MaritalDesc %in% c("married", "single")

#Created a subset of the original dataset to clean the data
final_employee_data<-subset(employee_data_projection, subset_bool)

#Check for missing values
summary(final_employee_data)

# group count of employees by position ordered by count
grouped_data <- final_employee_data %>%
  group_by(MaritalDesc) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Mean salary of employees by Marital Status
mean_salary_by_marital_status <- final_employee_data %>%
  group_by(MaritalDesc) %>%
  summarise(mean_salary = mean(Salary))

# Prepare the histogram of the salary distribution
salary_histogram <- ggplot(final_employee_data, aes(x = Salary)) +
  geom_histogram(binwidth = 5000, fill = "lightblue", color = "black", alpha = 0.7) +
  # Setup the title and axis labels
  labs(title = "Histogram of Annual Salary Distribution ($)",
       x = "Salaries ($)",
       y = "Frequency") +
  # Format x-axis labels with commas
  scale_x_continuous(labels = scales::comma) +
  # Set y-axis limits
  scale_y_continuous(limits = c(0, 70)) +
  # Add a bell curve
  stat_function(fun = function(x) {
    dnorm(x, mean = mean(final_employee_data[[INDEPENDENT_VARIABLE]]),
          sd = sd(final_employee_data[[INDEPENDENT_VARIABLE]])) * length(final_employee_data[[INDEPENDENT_VARIABLE]]) *
      diff(range(final_employee_data[[INDEPENDENT_VARIABLE]])) / 30
  }, aes(colour = "Bell Curve"), size = 1) +
  scale_color_manual(name = "Statistics", values = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Display the histogram
salary_histogram

# Save the histogram to a PNG file using ggsave
ggsave("salary_histogram.png", plot = salary_histogram, width = 8, height = 5, dpi = 300)

# Test for normality
shapiro.test(final_employee_data[[INDEPENDENT_VARIABLE]])

# Test for equality of variance
wilcox.test(final_employee_data[[INDEPENDENT_VARIABLE]] ~ final_employee_data[[DEPENDENT_VARIABLE]])

# Get the mean salary of the employees
mean_salary <- mean(final_employee_data[[INDEPENDENT_VARIABLE]])
mean_salary_stats_label <- paste("Mean Annual Salary ($): ", round(mean_salary, 2))

# Visualized the data using boxplot from the ggplot2 library
boxplot <- ggplot(final_employee_data, aes(x = MaritalDesc, y = Salary)) +
  # Create a boxplot with the Marital Description as the x-axis and Salary as the y-axis
  geom_boxplot(aes(fill = MaritalDesc), color = "black") +
  # Add a horizontal line for the mean salary
  geom_hline(aes(yintercept = mean_salary, color = mean_salary_stats_label), linetype = "dashed") +
  # Setup the title and axis labels
  labs(title = "Salary Based on Marital Description", x = "Marital Description", y = "Annual Salary ($)") +
  # Format y-axis labels with commas
  scale_fill_discrete(name = "Marital Description") +
  # Add a legend for the mean salary
  scale_color_manual(name = "Statistics", values = setNames("red", mean_salary_stats_label)) +
  # Set the legend title and plot title formatting
  theme(legend.title = element_text(face = "bold"), plot.title = element_text(hjust = 0.5))
boxplot

# Save the boxplot to a png file
ggsave("salary_boxplot.png", plot = boxplot)