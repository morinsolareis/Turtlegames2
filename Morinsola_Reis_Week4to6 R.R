## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library(tidyverse)
library(ggplot2)
library(reshape2)
library(dplyr)
install.packages("reshape2")
install.packages("pillar")
install.packages("tidyverse")

# Import the data set.
turtle_sales <- read.csv(file.choose(), header=T)

# Print the data frame.
turtle_sales
summary (turtle_sales)
str(turtle_sales)

# Use the summary() function.
summary(turtle_sales)

# Use the View() function
View(turtle_sales)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
turtle_sales2 <- subset(turtle_sales, select=-c(Ranking, Year, Genre, Publisher))



################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
library(ggplot2)
# Create scatterplots.

qplot(Global_Sales, EU_Sales, data=turtle_sales2,
      main='Scatterplot global sales vs EU sales')

qplot(Global_Sales, NA_Sales, data=turtle_sales2,
      main='Scatterplot global sales vs NA sales')

qplot(EU_Sales, NA_Sales, data=turtle_sales2,
      main='Scatterplot EU sales vs NA sales')

qplot(Global_Sales, Product, data=turtle_sales2,
      main='Scatterplot gobal sales by product')

## 2b) Histograms
# Create histograms.
qplot(Global_Sales, bins=25, data=turtle_sales2, main='Histogram global sales')
qplot(EU_Sales, bins=25, data=turtle_sales2, main='Histogram EU sales')
qplot(NA_Sales, bins=25, data=turtle_sales2, main='Histogram NA sales')
qplot(Product, bins=25, data=turtle_sales2, main='Histogram product by ID')

## 2c) Boxplots
# Create boxplots.
qplot(Global_Sales, data=turtle_sales2, colour=I('orange'), 
      main='Boxplot global sales', geom='boxplot')

qplot(EU_Sales, data=turtle_sales2, colour=I('orange'), 
      main='Boxplot EU sales', geom='boxplot')

qplot(NA_Sales, data=turtle_sales2, colour=I('orange'), 
      main='Boxplot NA sales', geom='boxplot')

qplot(Product, data=turtle_sales2, colour=I('orange'), 
      main='Boxplot Product ID sales', geom='boxplot')

# 3. Determine the impact on sales per product_id.

## 3a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
turtle_sales_group <- turtle_sales2 %>% group_by(Product,Genre) %>% summarise(across(.cols = c(NA_Sales, EU_Sales, Global_Sales), list(sum = sum))

                                                                              
                                                                              
###############################################################################

# 3. Observations and insights

## Your observations and insights here ......




###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and manipulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
View(turtle_sales2)
head(turtle_sales2)

# Check output: Determine the min, max, and mean values.
min(turtle_sales2$NA_Sales)
min(turtle_sales2$EU_Sales)
min(turtle_sales2$Global_Sales)

max(turtle_sales2$NA_Sales)
max(turtle_sales2$EU_Sales)
max(turtle_sales2$Global_Sales)

mean(turtle_sales2$NA_Sales)
mean(turtle_sales2$EU_Sales)
mean(turtle_sales2$Global_Sales)

# View the descriptive statistics.
summary(turtle_sales2)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
turtle_sales_sum <- turtle_sales2 %>% group_by(Product) %>% summarise(across(.cols = c(NA_Sales, EU_Sales, Global_Sales), list(sum = sum)))

# View the data frame.


# Explore the data frame.
summary(turtle_sales_sum)


## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
qplot(EU_Sales_sum, NA_Sales_sum, data=turtle_sales_sum)
qplot(EU_Sales_sum, Global_Sales_sum, data=turtle_sales_sum)
qplot(NA_Sales_sum, Global_Sales_sum, data=turtle_sales_sum)

# Create histograms.
qplot(EU_Sales_sum, bins=50, data=turtle_sales_sum)
qplot(NA_Sales_sum, bins=50, data=turtle_sales_sum)
qplot(Global_Sales_sum, bins=50, data=turtle_sales_sum)


# Create boxplots.
qplot(EU_Sales_sum, data=turtle_sales_sum, geom="boxplot")
qplot(NA_Sales_sum, data=turtle_sales_sum, geom="boxplot")
qplot(Global_Sales_sum, data=turtle_sales_sum, geom="boxplot")

###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
qplot(EU_Sales_sum, data=turtle_sales_sum, geom="boxplot")
qplot(NA_Sales_sum, data=turtle_sales_sum, geom="boxplot")
qplot(Global_Sales_sum, data=turtle_sales_sum, geom="boxplot")

# NA sales sum
# Create Q-Q plot
qqnorm(turtle_sales_sum$NA_Sales_sum)
# Add Q-Q line, which shows lack of normality
qqline(turtle_sales_sum$NA_Sales_sum)

# Global sales sum
# Create Q-Q plot
qqnorm(turtle_sales_sum$Global_Sales_sum)
# Add Q-Q line, which shows lack of normality
qqline(turtle_sales_sum$Global_Sales_sum)


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages("moments")
library(moments)
# Perform a Shapiro-Wilks test
shapiro.test((turtle_sales_sum$EU_Sales_sum))
shapiro.test((turtle_sales_sum$NA_Sales_sum))
shapiro.test((turtle_sales_sum$Global_Sales_sum))


## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
# EU
skewness(turtle_sales_sum$EU_Sales_sum) 
kurtosis(turtle_sales_sum$EU_Sales_sum) 

# NA
skewness(turtle_sales_sum$NA_Sales_sum)
kurtosis(turtle_sales_sum$NA_Sales_sum)

# Global
skewness(turtle_sales_sum$Global_Sales_sum)
kurtosis(turtle_sales_sum$Global_Sales_sum)

## 3d) Determine correlation
# Determine correlation.
# EU and NA sales_sum correlation
cor(turtle_sales_sum$EU_Sales_sum, turtle_sales_sum$NA_Sales_sum)
# EU and Global sales_sum correlation
cor(turtle_sales_sum$EU_Sales_sum, turtle_sales_sum$Global_Sales_sum)
# NA and Global sales_sum correlation
cor(turtle_sales_sum$NA_Sales_sum, turtle_sales_sum$Global_Sales_sum)

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.
ggplot(data=turtle_sales_sum,mapping=aes(x=Global_Sales_sum, y=NA_Sales_sum)) +
  geom_point(color='black',
             alpha=0.75,
             size=2.5) +
  geom_smooth(method='lm', color='orange') +
  scale_x_continuous("Global sales") +
  scale_y_continuous("NA sales") +
  labs(title="Turtle Games global sales vs NA sales (Million GBP)")

# EU sales sum Vs Global sales sum
ggplot(data=turtle_sales_sum, 
       mapping=aes(x=EU_Sales_sum, y=Global_Sales_sum)) + 
  geom_point(color = "blue", alpha = 0.5, size=2) + geom_smooth(method = "lm")

# EU sales sum Vs NA sales sum
ggplot(data=turtle_sales_sum, 
       mapping=aes(x=EU_Sales_sum, y=NA_Sales_sum)) + 
  geom_point(color = "black", alpha = 0.5, size=2)
#This has the lowest correlation so create a line of best fit 
ggplot(data=turtle_sales_sum, 
       mapping=aes(x=EU_Sales_sum, y=NA_Sales_sum)) + 
  geom_point(color="black", alpha = 0.5, size=2) + geom_smooth(method = "lm")

###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# View data frame created in Week 5.
df <- as_tibble(turtle_sales_sum)
# Explore data
View(df)
summary(df)

# Determine a summary of the data frame.
EU and NA sales_sum correlation = 0.62
cor(turtle_sales_sum$EU_Sales_sum, turtle_sales_sum$NA_Sales_sum)
# EU and Global sales_sum correlation = 0.85
cor(turtle_sales_sum$EU_Sales_sum, turtle_sales_sum$Global_Sales_sum)
# NA and Global sales_sum correlation = 0.92
cor(turtle_sales_sum$NA_Sales_sum, turtle_sales_sum$Global_Sales_sum)
###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
# EU vs Global sales_sum
model1 <- lm(Global_Sales_sum~EU_Sales_sum, data=df)
summary(model1)

# NA vs Global sales_sum
model2 <- lm(Global_Sales_sum~NA_Sales_sum, data=df)
summary(model2)


## 2b) Create a plot (simple linear regression)
# Basic visualisation.

plot(model1$residuals)
plot(df$NA_Sales_sum, df$Global_Sales_sum)
abline(coefficients(model2))

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
# columns (Global_Sales_sum, EU_Sales_sum, NA_Sales_sum)
# Multiple linear regression model.
model3 <- lm(Global_Sales_sum~EU_Sales_sum+NA_Sales_sum, data=df)
# View summary of model3
summary(model3)
# Adj R-squared: 97.09% of global sales explained by all variables.This suggests thr model will  e a good fit to predict global sales. 
# P-value is 2.2e-16: all variables are significant
# Pr(>|t|) is 8.24e-130: less than 0.05 so model fit is good


###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
# Create df
EU_Sales_sum <- c(23.80)
NA_Sales_sum<- c(34.02)
testpredict <- data.frame(EU_Sales_sum, NA_Sales_sum)
View(testpredict)

# A. NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80
# Predict Global_Sales_sum using testA data
predict(model3, newdata=testpredict, interval='confidence')
fit      lwr      upr
1 68.05655 66.42979 69.68331
# B. NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
EU_Sales_sum<- c(1.56)
NA_Sales_sum<- c(3.93)
test <- data.frame(EU_Sales_sum, NA_Sales_sum)
predict(model3, newdata=test, interval='confidence')
# fit      lwr     upr
# 7.356754 7.099418 7.61409
# C. NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65, observed value 4.32
EU_Sales_sum<- c(0.65)
NA_Sales_sum<- c(2.73)
test <- data.frame(EU_Sales_sum, NA_Sales_sum)
# Predict Global_Sales_sum using test1 data
predict(model3, newdata=test, interval='confidence')
# D. NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
EU_Sales_sum<- c(0.97)
NA_Sales_sum<- c(2.26)
test <- data.frame(EU_Sales_sum, NA_Sales_sum)
predict(model3, newdata=test, interval='confidence')
# E. NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52, Global sales 23.21
EU_Sales_sum<- c(0.52)
NA_sales_sum<- c(22.08)

###############################################################################

# 5. Observations and insights
# Your observations and insights here...
3 out of 5 predicted sales values appear close to actual values. 



###############################################################################
###############################################################################




