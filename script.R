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

# Parse all the string data in the dataset so that there are no leading or trailing whitespaces and convert to lowercase
for (i in seq_len(ncol(employee_data_projection))) {
  if (!is.character(employee_data_projection[[i]])) next

  employee_data_projection[[i]] <- trimws(employee_data_projection[[i]], which = "both")
  employee_data_projection[[i]] <- tolower(employee_data_projection[[i]])
}


subset_bool <- employee_data_projection$Department == "production" & employee_data_projection$MaritalDesc %in% c("married", "single")

#Created a subset of the original dataset to clean the data
employee_data<-subset(employee_data_projection, subset_bool)

#Check for missing values
summary(employee_data)

# group count of employees by position ordered by count
grouped_data <- employee_data %>%
  group_by(MaritalDesc) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Mean salary of employees by Marital Status
mean_salary_by_marital_status <- employee_data %>%
  group_by(MaritalDesc) %>%
  summarise(mean_salary = mean(Salary))

#adsgg

# Visualize the frequency of Salaries based on the cleaned dataset
hist(employee_data[[INDEPENDENT_VARIABLE]],
     main = "Histogram of Annual Salary Distribution ($)",
     xlab = "Salaries ($)",
     xaxt = "n",
     ylim = c(0, 70),
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

# Test for normality
shapiro.test(employee_data[[INDEPENDENT_VARIABLE]])

# Test for equality of variance
wilcox.test(employee_data[[INDEPENDENT_VARIABLE]] ~ employee_data[[DEPENDENT_VARIABLE]])

# Get the mean salary of the employees
mean_salary <- mean(employee_data[[INDEPENDENT_VARIABLE]])
mean_salary_stats_label <- paste("Mean Annual Salary ($): ", round(mean_salary, 2))

# Visualized the data using boxplot from the ggplot2 library
boxplot <- ggplot(employee_data, aes(x = MaritalDesc, y = Salary)) +
  geom_boxplot(aes(fill = MaritalDesc), color = "black") +
  geom_hline(aes(yintercept = mean_salary, color = mean_salary_stats_label), linetype = "dashed") +
  labs(title = "Salary Based on Marital Description", x = "Marital Description", y = "Annual Salary ($)") +
  scale_fill_discrete(name = "Marital Description") +
  scale_color_manual(name = "Statistics", values = setNames("red", mean_salary_stats_label)) +
  theme(legend.title = element_text(face = "bold"), plot.title = element_text(hjust = 0.5))
boxplot

# Save the boxplot to a png file
ggsave("salary_boxplot.png", plot = boxplot)