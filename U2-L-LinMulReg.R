# U2-L-Linear and Multi Regression

# SSE = sum of (actual value - predicted value)^2 for all of instances
# min of SSE is the better choice

# Sum of squared errors is hard to interpret for two reasons:
# 1. If we built the same model with twice as much data, the sum of squared errors might be 
# twice as big. But this doesn't mean it's a worse model. 
# 2. squared errors is in squared units of the dependent variable, so it's hard to understand.

# Because of these problems, Root Means Squared Error, or RMSE, is often used. it's normalized 
# by n and is in the same units as the dependent variable.
RMSE = sqrt(SSE/N)

# SST =The baseline model predicts the average value of the dependent variable regardless of the 
# value of the independent variable. The sum of squared errors for the baseline model is also 
# known as the total sum of squares, commonly referred to as SST.

# Another common error measure for linear regression is R squared. This error measure is
# nice because it compares the best model to a baseline model.

# R^2 = 1 - (SSE/SST)
# 0 < R^2 < 1 ----- closer to 1 is better

# R squared is nice because it's unitless and therefore universally interpretable between 
# problems. However,it can still be hard to compare between problems. Good models for easy 
# problems will have an R squared close to 1. But good models for hard problems can still have 
# an R squared close to zero.

# So which model should we use?
# Often not all variables should be used. This is because each additional variable used requires 
# more data, and using more variables creates a more complicated model. Overly complicated models 
# often cause what's known as overfitting. This is when you have a higher R squared on data used 
# to create the model, but bad performance on unseen data.

# Linear regression
setwd("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U2")
wine <- read.csv("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U2/wine.csv")
View(wine)
str(wine)
summary(wine)

# Linear Regression (one variable)
model1 = lm(Price ~ AGST, data=wine)
summary(model1)

# Call:
# lm(formula = Price ~ AGST, data = wine)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.78450 -0.23882 -0.03727  0.38992  0.90318 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -3.4178     2.4935  -1.371 0.183710    
# AGST          0.6351     0.1509   4.208 0.000335 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 0.4993 on 23 degrees of freedom
# Multiple R-squared:  0.435,	Adjusted R-squared:  0.4105 
# F-statistic: 17.71 on 1 and 23 DF,  p-value: 0.000335


# the coefficient for the intercept term, is estimated to be -3.4. And the coefficient for our 
# independent variable, is estimated to be 0.635.


# Multiple R-squared, 0.435 =  R^2
# Adjusted R-squared = adjusts the R-squared value to account for the number of independent 
# variables. Multiple R-squared will always increase if you add more independent variables. But 
# Adjusted R-squared will decrease if you add an independent variable that doesn't help the model. 
# This is a good way to determine if an additional variable should even be included in the model.


# Our residuals, or error terms, for all instances are stored in the vector model1$residuals.
# Sum of Squared Errors
model1$residuals
SSE = sum(model1$residuals^2)
SSE

# Linear Regression (two variables)
model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)

# Sum of Squared Errors
SSE = sum(model2$residuals^2)
SSE

# you can see that this variable really helped our model. Our Multiple R-squared and Adjusted 
# R-squared both increased significantly compared to the previous model. This indicates that this 
# new model is probably better than the previous model. Multiple R-squared: 0.7074,	Adjusted 
# R-squared: 0.6808 
 
# Linear Regression (all variables)
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)

# Sum of Squared Errors
SSE = sum(model3$residuals^2)
SSE

# If a coefficient is not significantly different from 0, then we should probably remove the
# variable from our model since it's not helping to predict the dependent variable.

# The standard error column gives a measure of how much the coefficient is likely to vary 
# from the estimate value.

# t value = Estimate/Std. Error 
# It will be negative if the estimate is negative and positive if the estimate is positive. The 
# larger the absolute value of the t value, the more likely the coefficient is to be significant. 
# So we want independent variables with a large absolute value in this column.


# Pr (>|t|)
# gives a measure of how plausible that the coefficient is actually 0, given the data we used to 
# build the model. This number will be large if the absolute value of the t value is small, and 
# it will be small if the absolute value of the t value is large. We want independent variables 
# with small values in this column.


# the easiest way in R to determine if a variable is significant is to look at the stars at the 
# end of each row. Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 When we ask 
# you to list the significant variables in a problem, we will usually not include '.'.

# Remove FrancePop
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)

# by removing the insignificant variables one by one in our model 
# Multiple R-squared:  0.8294,	Adjusted R-squared:  0.7845 
# reaches to 
# Multiple R-squared:  0.8286,	Adjusted R-squared:  0.7943
# which is stronger than the last model.


# what is correlation? Correlation measures the linear relationship between two variables and 
# is a number between -1 and +1. A correlation of +1 means a perfect positive linear relationship. 
# A correlation of -1 means a perfect negative linear relationship. a correlation of 0 means that 
# there is no linear relationship between the two variables. When we say that two variables are 
# highly correlated, we mean that the absolute value of the correlation is close to 1.

# compute the correlation between a pair of variablble
cor(wine$Age,wine$FrancePop)

# We can also compute the correlation between all pairs of variables in our data set 
cor(wine)

(cor(wine) > 0.7) | (cor(wine) < -0.7)
#              Year Price WinterRain  AGST HarvestRain   Age FrancePop
# Year         TRUE FALSE      FALSE FALSE       FALSE  TRUE      TRUE
# Price       FALSE  TRUE      FALSE FALSE       FALSE FALSE     FALSE
# WinterRain  FALSE FALSE       TRUE FALSE       FALSE FALSE     FALSE
# AGST        FALSE FALSE      FALSE  TRUE       FALSE FALSE     FALSE
# HarvestRain FALSE FALSE      FALSE FALSE        TRUE FALSE     FALSE
# Age          TRUE FALSE      FALSE FALSE       FALSE  TRUE      TRUE
# FrancePop    TRUE FALSE      FALSE FALSE       FALSE  TRUE      TRUE

# how does this information help us understand our linear regression model?
# We've confirmed that Age and FrancePopulation are definitely highly correlated. So we do have 
# multicollinearity problems. Keep in mind that multicollinearity refers to the situation when 
# two independent variables are highly correlated. A high correlation between an independent 
# variable and the dependent variable is a good thing since we're trying to predict the dependent 
# variable using the independent variables. But typically, a correlation greater than 0.7 or less 
# than -0.7 is cause for concern to make multicollinearity between independent variables.

# when we want to delete one of two correlate variables we see that which one is more related to
# the dependent variable. in the wine example price more related to age rather than the FrancePop
# so we deleted the second one from our model.


# Making Predictions

# Read in test set
wineTest = read.csv("wine_test.csv")
str(wineTest)

# The accuracy of the model on the test data is often referred to as out-of-sample accuracy. 
# To make predictions for these two test points, we'll use the predict function.
predictTest = predict(model4, newdata = wineTest)
predictTest

# we can quantify the accuracy of the model by computing the R-squared value for our test set.
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
R2 = 1 - SSE/SST

# The model R-squared is never negative since our model can't do worse on the training data than 
# the baseline model. However, our model can do worse on the test data compared to the baseline 
# model, leading to a negative R-squared value.

# When selecting a model, we want one with a good model R-squared but also with a good test set 
# R-squared. R^2 in test set can be negative because SSE can be more or less than SST, due to the 
# fact that this is an out-of-sample R² (test set R^2), not a model R².

