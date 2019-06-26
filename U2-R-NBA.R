# U2-R-NBA

# Read in the data
setwd("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U2/U2-Recitation2")
NBA = read.csv("NBA_train.csv")
View(NBA)
str(NBA)

# building a model and predict

# if a variable begins with a number in dataset, R will put an 'X' in front of it.

# How many wins to make the playoffs? 42 games
table(NBA$W, NBA$Playoffs)
#   29 12  0
#   30 19  1
#   31 15  1
#   32 12  0
#   33 17  0
#   34 16  0
#   35 13  3
#   36 17  4
#   37 15  4
#   38  8  7
#   39 10 10
#   40  9 13
#   41 11 26
#   42  8 29
#   43  2 18
#   44  2 27
#   45  3 22
#   46  1 15
#   47  0 28
#   48  1 14
#   49  0 17
#   50  0 32

# Can we use the difference between points scored and points allowed throughout the regular 
# season in order to predict the number of games that a team will win?
NBA$PTSdiff = NBA$PTS - NBA$oppPTS
cor(NBA$W, NBA$PTSdiff)
# [1] 0.9707433
winsReg = lm(NBA$W ~ NBA$PTSdiff , data = NBA)
summary(winsReg)

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.100e+01  1.059e-01   387.0   <2e-16 ***
# NBA$PTSdiff 3.259e-02  2.793e-04   116.7   <2e-16 ***

w = 41 + 0.0326 * PTSdiff & w >= 42 --> PTSdiff = 30.67
# So we need to score at least 31 more points than we allow in order to win at least 42 games.

# now let's build an equation to predict points scored using some common basketball statistics. 
# Linear regression model for points scored
PointsReg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data=NBA)
summary(pointReg)

# We do have a pretty good R-squared value, 0.8992, so it shows that there really is a linear 
# relationship between points and all of these basketball statistics.

# Sum of Squared Errors
PointsReg$residuals
SSE = sum(PointsReg$residuals^2)
SSE
# [1] 28394314

# SSE is not an interpretable quantity. so a more interpretable error is
# Root mean squared error
RMSE = sqrt(SSE/nrow(NBA))
RMSE
# [1] 184.4049

# So on average, we make an error of about 184.4 points. That seems like quite a lot, until you 
# remember that the average number of points in a season is, let's see, mean(NBA$PTS) = 8,370. 

# Average number of points in a season
mean(NBA$PTS)
# [1] 8370

# Remove insignifcant variables
summary(PointsReg)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.051e+03  2.035e+02 -10.078   <2e-16 ***
# X2PA         1.043e+00  2.957e-02  35.274   <2e-16 ***
# X3PA         1.259e+00  3.843e-02  32.747   <2e-16 ***
# FTA          1.128e+00  3.373e-02  33.440   <2e-16 ***
# AST          8.858e-01  4.396e-02  20.150   <2e-16 ***
# ORB         -9.554e-01  7.792e-02 -12.261   <2e-16 ***
# DRB          3.883e-02  6.157e-02   0.631   0.5285    
# TOV         -2.475e-02  6.118e-02  -0.405   0.6859    
# STL         -1.992e-01  9.181e-02  -2.169   0.0303 *  
# BLK         -5.576e-02  8.782e-02  -0.635   0.5256    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 185.5 on 825 degrees of freedom
# Multiple R-squared:  0.8992,	Adjusted R-squared:  0.8981 
# F-statistic: 817.3 on 9 and 825 DF,  p-value: < 2.2e-16

# for a better model remove some of the insignificant variables one at a time.
# The first variable we would want to remove is probably turnovers. And why do I say turnovers? 
# because the p value for turnovers, 0.6859, is the highest of all of the p values. So that means 
# that turnovers is the least statistically significant variable in our model. 

PointsReg2 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data=NBA)
summary(PointsReg2)

# Call:
# lm(formula = PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + 
#     BLK, data = NBA)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -526.79 -121.09    6.37  120.74  565.94 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.077e+03  1.931e+02 -10.755   <2e-16 ***
# X2PA         1.044e+00  2.951e-02  35.366   <2e-16 ***
# X3PA         1.263e+00  3.703e-02  34.099   <2e-16 ***
# FTA          1.125e+00  3.308e-02  34.023   <2e-16 ***
# AST          8.861e-01  4.393e-02  20.173   <2e-16 ***
# ORB         -9.581e-01  7.758e-02 -12.350   <2e-16 ***
# DRB          3.892e-02  6.154e-02   0.632   0.5273    
# STL         -2.068e-01  8.984e-02  -2.301   0.0216 *  
# BLK         -5.863e-02  8.749e-02  -0.670   0.5029    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 185.4 on 826 degrees of freedom
# Multiple R-squared:  0.8991,	Adjusted R-squared:  0.8982 
# F-statistic: 920.4 on 8 and 826 DF,  p-value: < 2.2e-16

PointsReg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data=NBA)
summary(PointsReg3)

# Call:
# lm(formula = PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, 
#     data = NBA)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -523.79 -121.64    6.07  120.81  573.64 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.015e+03  1.670e+02 -12.068  < 2e-16 ***
# X2PA         1.048e+00  2.852e-02  36.753  < 2e-16 ***
# X3PA         1.271e+00  3.475e-02  36.568  < 2e-16 ***
# FTA          1.128e+00  3.270e-02  34.506  < 2e-16 ***
# AST          8.909e-01  4.326e-02  20.597  < 2e-16 ***
# ORB         -9.702e-01  7.519e-02 -12.903  < 2e-16 ***
# STL         -2.276e-01  8.356e-02  -2.724  0.00659 ** 
# BLK         -3.882e-02  8.165e-02  -0.475  0.63462    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 185.4 on 827 degrees of freedom
# Multiple R-squared:  0.8991,	Adjusted R-squared:  0.8982 
# F-statistic:  1053 on 7 and 827 DF,  p-value: < 2.2e-16

PointsReg4 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data=NBA)
summary(PointsReg4)

# Call:
# lm(formula = PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = NBA)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -523.33 -122.02    6.93  120.68  568.26 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.033e+03  1.629e+02 -12.475  < 2e-16 ***
# X2PA         1.050e+00  2.829e-02  37.117  < 2e-16 ***
# X3PA         1.273e+00  3.441e-02  37.001  < 2e-16 ***
# FTA          1.127e+00  3.260e-02  34.581  < 2e-16 ***
# AST          8.884e-01  4.292e-02  20.701  < 2e-16 ***
# ORB         -9.743e-01  7.465e-02 -13.051  < 2e-16 ***
# STL         -2.268e-01  8.350e-02  -2.717  0.00673 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 185.3 on 828 degrees of freedom
# Multiple R-squared:  0.8991,	Adjusted R-squared:  0.8983 
# F-statistic:  1229 on 6 and 828 DF,  p-value: < 2.2e-16

# Compute SSE and RMSE for new model
SSE4 = sum(PointsReg4$residuals^2)
RMSE4 = sqrt(SSE4/nrow(NBA))
SSE4
RMSE4


# So it seems like we've narrowed down on a much better model because it's simpler, it's more 
# interpretable, and it's got just about the same amount of error.


#  we'll try to make predictions for the 2012-2013 season.

# our training set only included data from 1980 up until the 2011-2012 season.
# We'll need to load our test set because it contains the data for 2012-2013

NBA_test = read.csv("NBA_test.csv")
pointsPredictions = predict(PointsReg4, newdata = NBA_test)

# so now that we have our prediction, how good is it? We can compute the out of sample R-squared. 
# This is a measurement of how well the model predicts on test data. The R-squared value we had 
# before from our model, the 0.8991, you might remember, is the measure of an in-sample R-squared, 
# which is how well the model fits the training data. But to get a measure of the predictions 
# goodness of fit, we need to calculate the out of sample R-squared.


# Compute out-of-sample R^2
SSE = sum((NBA_test$PTS - pointsPredictions)^2)
SST = sum((NBA_test$PTS - mean(NBA$PTS))^2)
R2 = 1 - SSE/SST
R2
# [1] 0.8127142

# Compute the RMSE
RMSE = sqrt(SSE/nrow(NBA_test))
RMSE
# [1] 196.3723