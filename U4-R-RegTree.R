# U4-R-Regression Trees

# we will be looking at regression trees, and applying them to data related to house prices and 
# locations. we mostly discuss classification trees with the output as a factor or a category. 
# Trees can also be used for regression tasks. The output at each leaf of a tree is no longer a 
# category, but a number. Just like classification trees, regression trees can capture 
# nonlinearities that linear regression can't.

# with classification trees we report the average outcome at each leaf of our tree. For example, 
# if the outcome is true 15 times, and false 5 times, the value at that leaf of a tree would be 
# 15/(15+5)=0.75. Now, if we use the default threshold of 0.5, we would say the value at this 
# leaf is true. With regression trees, we now have continuous variables. So instead of-- we 
# report the average of the values at that leaf. So suppose we had the values 3, 4, and 5 at 
# one of the leaves of our trees. Well, we just take the average of these numbers, which is 4.

# We are interested in building a model initially of how prices vary by location across a region.
setwd("C:/Users/Administrator/Desktop/R Projects/unit4/recitation 4")
boston <- read.csv("C:/Users/Administrator/Desktop/R Projects/unit4/recitation 4/boston.csv")
View(boston)
plot(boston$LON , boston$LAT)

#  we want to show all the points that lie along the Charles River in a different color. We have 
#  a variable, CHAS, that tells us if a point is on the Charles River or not. So to put points on 
#  an already-existing plot, we can use the points command, which looks very similar to the plot 
#  command, except it operates on a plot that already exists.  
points(boston$LON[boston$CHAS==1] , boston$LAT[boston$CHAS==1], col = "blue" , pch = 19)

# if you'd like to know where MIT is in this picture.
points(boston$LON[boston$TRACT==3531] , boston$LAT[boston$TRACT==3531], col = "red" , pch = 19)

summary(boston$NOX)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.3850  0.4490  0.5380  0.5547  0.6240  0.8710 

# mean == 0.55
# those are all the points that have got above-average pollution.
points(boston$LON[boston$NOX >= 0.55] , boston$LAT[boston$NOX >= 0.55], col = "green" , pch = 19)

# If we look at the distribution of the housing prices (boston$MEDV), thousands of dollars,
plot(boston$LON , boston$LAT)
summary(boston$MEDV)

#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    5.00   17.02   21.20   22.53   25.00   50.00 

# So let's plot again only the above-average price points.
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=20)

# what does the linear regression model think is above median.
latlonlm = lm(MEDV ~ LAT  + LON , data = boston) 
# this is what the linear regression model predicts for each of the 506 census tracts.
latlonlm$fitted.values
points(boston$LON[latlonlm$fitted.values >= 21.2] , boston$LAT[latlonlm$fitted.values >= 21.2], col = "blue" , pch = "$")

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -3178.472    484.937  -6.554 1.39e-10 ***
# LAT             8.046      6.327   1.272    0.204    
# LON           -40.268      5.184  -7.768 4.50e-14 ***
# you can see the sharp line that the linear regression defines. And how it's pretty much 
# vertical, because the latitude variable was not very significant in the regression.
# So the linear regression model isn't really doing a good job. And it's completely ignored 
# everything to the right side of the picture.


# Regression Tree


library(rpart)
library(rpart.plot)
latlonTree = rpart(MEDV ~ LAT + LON , data = boston)
prp(latlonTree)

# the important thing is to look at the leaves. In a classification tree, the leaves would be 
# the classification we assign that these splits would apply to. But in regression trees, we 
# instead predict the number. That number is the average of the median house prices in that 
# bucket or leaf.

# Now we want to predict what the tree thinks is above median, just like we did with linear 
# regression. So we'll say the fitted values we can get from using the predict command on the 
# tree we just built.

# Visualize output
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=20)

fittedvalues = predict(latlonTree)
points(boston$LON[fittedvalues >= 21.2] , boston$LAT[fittedvalues >= 21.2], col = "blue" , pch = "$")

# Now we see that we've done a much better job than linear regression was able to do.  
# but the tree was very complicated. So maybe it's drastically overfitting. Can we get most of 
# this effect with a much simpler tree? We can.
latlonTree = rpart(MEDV ~ LAT + LON , data = boston , minbucket = 50)
plot(latlonTree)
text(latlonTree)
# We'll use the other way of plotting trees, plot, and we'll add text to the text command. And 
# we see we have far fewer splits, and it's far more interpretable.

# Visualize output of minbucket 50
plot(boston$LON , boston$LAT)
abline(v= -71.07)
abline(h= 42.21)
abline(h= 42.17)
points(boston$LON[boston$MEDV >= 21.2] , boston$LAT[boston$MEDV >= 21.2], col = "red" , 
pch = 19)

# We've correctly shown how the regression tree carves out that rectangle in the bottom of 
# Boston and says that is a low value area.

# comparison between 2 trees
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=20)
points(boston$LON[fittedvalues >= 21.2] , boston$LAT[fittedvalues >= 21.2], col = "blue" , pch = "!")
latlonTree = rpart(MEDV ~ LAT + LON , data = boston , minbucket = 50)
fittedvalues = predict(latlonTree)
points(boston$LON[fittedvalues >= 21.2] , boston$LAT[fittedvalues >= 21.2], col = "green" , pch = "*")



# We're going to try to predict house prices using all the variables we have available to us.
library(caTools)
set.seed(123)
split = sample.split(boston$MEDV, SplitRatio = 0.7)
train = subset(boston, split == TRUE)
test = subset(boston, split == FALSE)
linreg = lm(MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
summary(linreg)

# Call:
# lm(formula = MEDV ~ . - TOWN - TRACT - MEDV, data = train)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -14.511  -2.712  -0.676   1.793  36.883 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.523e+02  4.367e+02  -0.578   0.5638    
# LON         -2.987e+00  4.786e+00  -0.624   0.5329    
# LAT          1.544e+00  5.192e+00   0.297   0.7664    
# CRIM        -1.808e-01  4.390e-02  -4.118 4.77e-05 ***
# ZN           3.250e-02  1.877e-02   1.731   0.0843 .  
# INDUS       -4.297e-02  8.473e-02  -0.507   0.6124    
# CHAS         2.904e+00  1.220e+00   2.380   0.0178 *  
# NOX         -2.161e+01  5.414e+00  -3.992 7.98e-05 ***
# RM           6.284e+00  4.827e-01  13.019  < 2e-16 ***
# AGE         -4.430e-02  1.785e-02  -2.482   0.0135 *  
# DIS         -1.577e+00  2.842e-01  -5.551 5.63e-08 ***
# RAD          2.451e-01  9.728e-02   2.519   0.0122 *  
# TAX         -1.112e-02  5.452e-03  -2.040   0.0421 *  
# PTRATIO     -9.835e-01  1.939e-01  -5.072 6.38e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 5.595 on 350 degrees of freedom
# Multiple R-squared:  0.665,	Adjusted R-squared:  0.6525 
# F-statistic: 53.43 on 13 and 350 DF,  p-value: < 2.2e-16

# So we see that the latitude and longitude are not significant for the linear regression, 
# Crime is very important and has a negative impact on the price of house

linreg.pred = predict(linreg, newdata = test)
linreg.sse = sum((linreg.pred-test$MEDV)^2)
linreg.sse
# 3037.088

# can we beat this using regression trees?

tree = rpart(MEDV ~ LON + LAT + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
prp(tree)
tree.pred = predict(tree, newdata = test)
tree.sse = sum((tree.pred-test$MEDV)^2)
tree.sse
# 4328.988

# regression trees are not as good as linear regression for this problem.

# Let us define RSS to be the residual sum of squares, also known as the sum of square 
# differences. Our goal when building the tree is to minimize the RSS by making splits, but we 
# want to penalize having too many splits now.

# CP

# Now remember that cp varies between 0 and 1. It's likely for any given problem that we don't 
# need to explore the whole range.
# that small numbers of cp encourage large trees, and large values of cp encourage small trees.
library(caret)
library(e1071)

tr.control=trainControl(method="cv", number=10)
cp.grid = expand.grid(.cp = seq(0,0.01,0.001))
tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS+ RAD + TAX + PTRATIO , data = train , method = "rpart" , trControl = tr.control , tuneGrid = cp.grid)
tr


# CART 

# 364 samples
#  13 predictor

# No pre-processing
# Resampling: Cross-Validated (10 fold) 
# Summary of sample sizes: 328, 327, 327, 327, 327, 328, ... 
# Resampling results across tuning parameters:

#   cp     RMSE      Rsquared   MAE     
#   0.000  4.775385  0.7440657  3.163776
#   0.001  4.771538  0.7444303  3.168201
#   0.002  4.830608  0.7384188  3.256068
#   0.003  4.885041  0.7319225  3.277138
#   0.004  4.888427  0.7324738  3.294059
#   0.005  4.966115  0.7243772  3.305364
#   0.006  4.936891  0.7267991  3.314405
#   0.007  4.939064  0.7267008  3.362739
#   0.008  4.935690  0.7269420  3.354972
#   0.009  4.921955  0.7272466  3.320888
#   0.010  4.921955  0.7272466  3.320888

# RMSE was used to select the optimal model using the smallest value.
# The final value used for the model was cp = 0.001.

# cp equals 0.001 was the best because it had the best RMSE-- Root Mean Square Error.

# So let's see what the tree that that value of cp corresponds to is. So we can get that from 
# going best.tree=tr$finalModel. And we can plot that tree. So that's the model that corresponds 
# to 0.001. Plot it.

best.tree = tr$finalModel
prp(best.tree)

best.tree.pred = predict(best.tree, newdata = test)
best.tree.sse = sum((best.tree.pred - test$MEDV)^2)
best.tree.sse
# 3675.766

# This tree is better on the testing set than the original tree we created. But, you may also 
# remember that the linear regression model did actually better than that still. The linear 
# regression SSE was more around 3,030. So the best tree is not as good as the linear regression 
# model. But cross validation did improve performance. So the takeaway is, I guess, that trees 
# aren't always the best method you have available to you. But you should always try cross 
# validating them to get as much performance out of them as you can.