# KINESSO EMPLOYMENT TEST ####
## 1 Import Sales Data ####
install.packages("tidyverse")
library(tidyverse)
library(readxl)
data <- read_xlsx('toy_sales_data.xlsx',sheet = 'data')
data
planned_spend <- read_xlsx('toy_sales_data.xlsx', sheet = 'planned_spend')
planned_spend

## 2 Create A Plot of Sales, Tv And Digital Investment In The Y Axis With Time In The X Axis ####
spend_and_sales <- data %>% select(2:4)
color = rainbow(3)
ts.plot(spend_and_sales,col=color, ylab='Sales and Investment ($)', 
        xlab='Time (Months)', main = "Investment and sales over time")
legend("topleft", legend = colnames(spend_and_sales), lty=1, col=color)
abline(v=5)
abline(v=12)
abline(v=16)
abline(v=20)

## 3 Report On The Correlations Among Sales Tv And Digital Investments ####
# We can see from the graph that there is some positive correlation between 
# sales and spend as shown in the graph by the many of the peaks in sales 
# corresponding to a peak in one of the spend amounts as demonstrated by the 
# 5th, 12th, 16th and 20th months. The level of correlation will need further 
# investigation.

## 4 Fit A Regression Model On The Data
fit <- lm(data$sales~.,data = data)
summary(fit)

## 4a Report On The Adjusted R-squared ####
# The adjusted R squared for this model is 0.8985. This means that 89.85% of the
# variance in sales is explained by the change in the variables provided. This 
# is a  decent output for a linear model.

## 4b Report On The P-value And Significance Of Each Regressor ####
# The p-values of the intercept and the month and trend variables are not 
# statistically significant each at a value of around 0.3 meaning we are only 
# 70% confident of the coefficient value not being zero. The p-value of the xmas
# variable is 0.00563, which means we are 99.437% confident of the significance
# of the coefficient. This level would be satisfactory in the majority of 
# situations. Finally the p-value for the tv_spend and digital_spend are both 
# below 0.001, giving them a confidence level of their significance of above 
# 99.9% which puts them well into the statistically significant range.

## 5 Calculate The Contribution From Tv Spend To Sales In % And Absolute Dollar Value ####
(1.947/(-5.287+1.947+2.872+1.419e+07+3.097e+06))*100
# Therefore tv_spend contributes 0.000112628% to the total contribution towards 
# sales.
# In terms of absolute dollar amount, it contributes $1.947 in sales for each 
# additional dollar spent.

## 6 Calculate Tv ROI ####
total_sales =sum(data$sales)
total_spend = sum(data$tv_spend)
ROI = ((total_sales-total_spend)/total_spend)*100
ROI
# Therefore the total return on investment for the 2 year period is 943.0519%.

## 7 Calculate The Expected Value For The First 3 Months Of 2018 ####
planned_spend$trend <- c(25,26,27)
planned_spend$xmas <- c(0,0,0)
planned_spend
predict.lm(fit, newdata = planned_spend)
# Therefore, for the first three months of 2018 the model predicts $11,750,228 
# for January, $12,760,264 for February and $15,656,009 for March.

## 8 What Additional Data Would Improve Your Model ####
# There are a number of additional variables that could improve the model. In 
# terms of the tv_spend, it would be useful to know what level of the products 
# target audience (which could be found through unsupervised clustering) was 
#reached along with the frequency according to tv ratings. This could be 
# broken down into the percentages of the audience that  would have seen the ad
# multiple times from once to 5 time
#
# In terms of digital_spend, variables like the click through rate to measure 
# the percentage who clicked on the ad as well as tracking through what 
# percentage of those clicks led to sales would allow us to better understand 
# the impact of the tv vs digital advertisements. A breakdown of spending on 
# each social media or ad services would also be useful to see the impact of 
# spending on different services.
#
# Some investment for both TV and digital may also have a delayed payout. If an 
# ad is launched late in October we may not see sales until November. Because of
# this, it could be useful to include extra variables carrying over the previous 
# months advertising spend.
#
# It would also be useful to know when new toy lines are being released as this 
# could result in a boosted sales level for the release month and well the 
# subsequent months having variables similar to xmas telling the model if it has
# been 1, 2, or 3 months since a major release could indicate this to the model.
# Similar indicators could be used for expansion into new markets and retailers.
# Finally, price discounts could also influence sales. Variables indicating the 
# lowest selling price for each month either as a price or as a percentage 
# discount could be used to convey this to the model.