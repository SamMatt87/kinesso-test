# KINESSO EMPLOYMENT TEST ####
## Import Sales Data ####
install.packages("tidyverse")
library(tidyverse)
library(readxl)
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
# We can see from the graph that there is some positive correlation between sales and spend as shown 
#by many of the peaks in sales corresponding to a peak in one of the spend amounts as shown in the 5th, 
#12th, 16th and 20th months. The level of correlation will need further investigation.

## Fit A Reggression Model On The Data
fit <- lm(data$sales~.,data = data)
summary(fit)

## Report On The Adjusted R-squared ####
# The adjusted R squared for this model is 0.8985. This means that 89.85% of the variance in sales is 
# explained by the change in these variables. This is a relatively high output for a linear model.

## Report On The P-value And Significance Of Each Regressor ####
# The p-values of the intercept and the month and trend variables are not statistically significant each
# at a value of around 0.3 meaning we are only 70% confident of their significance. The p-value of the
# xmas variable is 0.00563, which means we are 99.437% confident of its significance. This level would 
# be satisfactory in the majority of situations. Finally the p-value for the tv_spend and digital_spend
# are both below 0.001, giving them a confidence level of above 99.9% which puts them well into the 
# statistically significant range.

## Calculate The Contribution From Tv Spend To Sales In % And Absolute Dollar Value ####
(1.947/(-5.287+1.947+2.872+1.419e+07+3.097e+06))*100
# Therefore tv_spend contributes 0.000112628% to the total contribution towards sales
# In terms of absolute dollar amount, it contributes $1.947 for every extra dollar spent

## Calculate Tv ROI ####
total_sales =sum(data$sales)
total_spend = sum(data$tv_spend)
ROI = (total_sales/total_spend)*100
ROI

## Calculate The Expected Value For The First 3 Months Of 2018
planned_spend$trend <- c(25,26,27)
planned_spend$xmas <- c(0,0,0)
planned_spend
predict.lm(fit, newdata = planned_spend)

## What Additional Data Would Improve Your Model
# 

