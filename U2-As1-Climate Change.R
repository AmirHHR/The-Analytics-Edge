# U2-As1-Climate Change

# We are interested in how changes in these variables affect future temperatures, as well as how 
# well these variables explain temperature changes so far.

# build a linear regression model to predict the dependent variable Temp, using MEI, CO2, CH4, 
# N2O, CFC.11, CFC.12, TSI, and Aerosols as independent variables and Enter the model R2
setwd("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U2/U2-As1-Climate Change")
climate <- read.csv("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U2/U2-As1-Climate Change/climate_change.csv")
str(climate)
climatelm = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)
summary(climatelm)
# 0.75

# Which variables are significant in the model? only the p-value is below 0.05.
# MEI, CO2, CFC.11, CFC.12, TSI, and Aerosols 

# Which of the independent variables is N2O highly correlated with? |cor| > 0.7
cor(train)

# R provides a function, step, that will automate the procedure of trying different combinations 
# of variables to find a good compromise of model simplicity and R2. it can be informally thought 
# of as the quality of the model with a penalty for the number of variables in the model.
# The step function has one argument - the name of the initial model. 
climateStep = step(climatelm)

# It is interesting to note that the step function does not address the collinearity of the 
# variables, except that adding highly correlated variables will not improve the R2 significantly. 
# The consequence of this is that the step function will not necessarily produce a very 
# interpretable model - just a model that has balanced quality and simplicity for a particular 
# weighting of quality and simplicity (AIC).

summary(StepModel)

# Call:
# lm(formula = Temp ~ MEI + CO2 + N2O + CFC.11 + CFC.12 + TSI + 
#     Aerosols, data = train)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.25770 -0.05994 -0.00104  0.05588  0.32203 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.245e+02  1.985e+01  -6.273 1.37e-09 ***
# MEI          6.407e-02  6.434e-03   9.958  < 2e-16 ***
# CO2          6.402e-03  2.269e-03   2.821 0.005129 ** 
# N2O         -1.602e-02  8.287e-03  -1.933 0.054234 .  
# CFC.11      -6.609e-03  1.621e-03  -4.078 5.95e-05 ***
# CFC.12       3.868e-03  9.812e-04   3.942 0.000103 ***
# TSI          9.312e-02  1.473e-02   6.322 1.04e-09 ***
# Aerosols    -1.540e+00  2.126e-01  -7.244 4.36e-12 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 0.09155 on 276 degrees of freedom
# Multiple R-squared:  0.7508,	Adjusted R-squared:  0.7445 
# F-statistic: 118.8 on 7 and 276 DF,  p-value: < 2.2e-16

# Using the step function model, calculate temperature predictions for the testing data set
tempPredict = predict(climateStep, newdata = test)
SSE = sum((tempPredict - test$Temp)^2)
SST = sum( (mean(train$Temp) - test$Temp)^2)
R2 = 1 - SSE/SST
R2
# [1] 0.6286051


