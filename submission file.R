# KINESSO EMPLOYMENT TEST ####
## Import Sales Data ####
install.packages("tidyverse")
library(tidyverse)
data <- readxl_example("toy_sales_data.xlsx")
library("readxl")
data <- read_xlsx('toy_sales_data.xlsx',sheet = 'data')
data
planned_spend <- read_xlsx('toy_sales_data.xlsx', sheet = 'planned_spend')
planned_spend

## Create A Plot of Sales, Tv And Digital Investment In The Y Axis With Time In The X Axis ####
spend_and_sales <- data %>% select(2:4)
color = rainbow(3)
ts.plot(spend_and_sales,col=color)
legend("topleft", legend = colnames(spend_and_sales), lty=1, col=color)
abline(v=5)
abline(v=16)
abline(v=20)
abline(v=12)

## Report On The Correlations Among Sales Tv And Digital Investments ####
# We can see from the graph that there is a weak correlation between sales and spend as shown by many of
# the peaks in sales corresponding to a peak in one of the spend amounts as shown in the 5th, 12th, 16th
# and 20th months. The level of correlation will need further investigation.

## Fit A Reggression Model On The Data
fit <- lm(data$sales~.,data = data)
summary(fit)

## Report On The Adjusted R-squared ####
# The adjusted R squared for this model is 0.8985. This means that 89.85% of the variance in sales is 
# explained by the change in these variables. This is a relatively high output for a linear model.

## Report On The P-value And Significance Of Each Regressor ####