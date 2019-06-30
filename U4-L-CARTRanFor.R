# U4-L-CART and Random Forest

# CART

# logistic regression models are not easily interpretable. The model coefficients in logistic 
# regression indicate the importance and relative effect of variables, but do not give a simple 
# explanation of how a decision is made.

# To predict the outcome for a new observation or case, you can follow the splits in the tree 
# and at the end, you predict the most frequent outcome in the training set that followed the 
# same path.

# Some advantages of CART are that it does not assume a linear model, like logistic regression 
# or linear regression, and it's a very interpretable model.

# Classification and regression trees, CART builds what is called a tree by splitting on the 
# values of the independent variables. To predict the outcome for a new observation or case, you 
# can follow the splits in the tree and at the end, you predict the most frequent outcome. In 
# trees, a yes response is always to the left and a no response is always to the right. Also, 
# make sure you always start at the top of the tree.

# One way to control how many splits are generated is by setting a lower bound for the number of 
# data points in each subset. In R, this is called the minbucket parameter for the minimum number of observations
# in each bucket or subset.The smaller minbucket is, the more splits will be generated. But if 
# it's too small, overfitting will occur. On the other hand, if the minbucket parameteris too 
# large, the model will be too simpleand the accuracy will be poor.

# Instead of just taking the majority outcome to be the prediction, we can compute the percentage 
# of data in a subset of each type of outcome. As an example, if we have a subset with 10 affirms 
# and two reverses, then 87% of the datais affirm.Then, just like in logistic regression, we can 
# use a threshold value to obtain our prediction.

# This is a binary variable taking value 1 if Justice Stevens decided to reverse or overturn the 
# lower court decision, and taking value 0 if Justice Stevens voted to affirm or maintain the 
# lower court decision.

# Data Frame descriptions
# Docket is just a unique identifier for each case
# Term is the year of the case 
# variables: 
# the circuit court of origin
# the issue area of the case
# the type of petitioner
# the type of respondent
# the lower court direction
# whether or not the petitioner argued that a law or practice was unconstitutional
# The last variable is our dependent variable, whether or not Justice Stevens voted to reverse 
# the case: 1 for reverse and 0 for affirm.

install.packages("rpart.plot")
install.packages("rpart")

setwd("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U4/lesson")
stevens <- read.csv("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U4/lesson/stevens.csv")
View(stevens)
str(stevens)

library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse , SplitRatio = 0.7)
Train = subset(stevens, spl == TRUE)
Test = subset(stevens, spl == FALSE)
library(rpart)
library(rpart.plot)

# method = "class" tells rpart to build a classification tree, instead of a regression tree. 
# minbucket limits the tree so that it doesn't overfit to our training set

stevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
data = Train , method = "class" , minbucket = 25)
prp(stevensTree)

# prp will select the abbreviation so that they're uniquely identifiable. So if you made a table, 
# you could see that CRI stands for criminal defendant, INJ stands for injured person, etc. A 
# CART tree is a series of decision rules which can easily be explained.

predictCART = predict(stevensTree, newdata = Test, type = "class")
# 1   3   4   6   8  21  32  36  40  42  46  47  53  55  59  60  66  67  68  72 
# 1   1   1   1   1   1   0   0   1   0   0   1   0   1   1   1   1   1   1   1 
# ...

# We need to give this argument when making predictions for our CART model if we want the 
# majority class predictions. This is like using a threshold of 0.5.

# Now let's compute the accuracy of our model by building a confusion matrix.
table(Test$Reverse, predictCART)

#    predictCART
#      0  1
#   0 41 36
#   1 22 71

# accuracy
# (41+71)/(41+36+22+71) = 0.6588235

# If you were to build a logistic regression model, you would get an accuracy of 0.665 and a 
# baseline model that always predicts Reverse, the most common outcome, has an accuracy of 0.547.

# CART is as good as logistic regression but It's much more interpretable than it


# evaluate the model
library(ROCR)
predictROC = predict(stevensTree, newdata = Test)
predictROC
#             0         1
# 1   0.3035714 0.6964286
# 3   0.3035714 0.6964286
# 4   0.4000000 0.6000000
# 6   0.4000000 0.6000000
# 8   0.4000000 0.6000000
# ...

# For each observation in the test set, it gives two numbers which can be thought of as the 
# probability of outcome 0 and the probability of outcome 1. More concretely, each test set 
# observation is classified into a bucket of our CART tree. These numbers give the percentage 
# of training set data in that subset with outcome 0 and the percentage of data in the training 
# set in that subset with outcome 1. We'll use the second column as our probabilities to 
# generate an ROC curve.We'll use the second column as our probabilities to generate an ROC curve.

pred = prediction(predictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)

# Compute the AUC of the CART model
as.numeric(performance(pred, "auc")@y.values)
# [1] 0.6927105

# recall that our last tree had 7 splits. 
# You can build a CART model with minbucket=5 by using the following command: 
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
method="class", data = Train, minbucket=5) 
prp(StevensTree) 
# you can see that the tree has 16 splits! This tree is probably overfit to the training data, 
# and is not as interpretable.

StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
method="class", data = Train, minbucket=100)
prp(StevensTree)
# you can see that the tree only has one split! This tree is probably not fit well enough to 
# the training data.



# Random Forests

# This method was designed to improve the prediction accuracy of CART and works by building a 
# large number of CART trees. Unfortunately, this makes the method less interpretable than CART, 
# so often you need to decide if you value the interpretability or the increase in accuracy more.

# To make a prediction for a new observation, each tree in the forest votes on the outcome and we 
# pick the outcome that receives the majority of the votes.

# So how does random forests build many CART trees? We can't just run CART multiple times because 
# it would create the same tree every time. To prevent this, random forests only allows each tree 
# to split on a random subset of the available independent variables, and each tree is built from 
# what we call a bagged or bootstrapped sample of the data.This just means that the data used as 
# the training data for each tree is selected randomly with replacement.

# The first parameter in random forest is the minimum number of observations in a subset which is 
# called nodesize. A smaller value of nodesize, which leads to bigger trees, may take longer in R.
# Random forests lead to much more computationally intensive than CART. The second parameter is 
# the number of trees to build, which is called ntree in R. This should not be set too small, but 
# the larger it is the longer it will take. A nice thing about random forests is that it's not as 
# sensitive to the parameter values as CART is.

# stevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt
# + Unconst , data = Train , nodesize = 25 , ntree = 200)

# You should see an interesting warning message here. In CART, we added the argument method=
# "class", so that it was clear that we're doing a classification problem. As I mentioned earlier, 
# trees can also be used for regression problems. The randomForest function does not have a 
# method argument. So when we want to do a classification problem, we need to make sure outcome 
# is a factor. Let's convert the variable Reverse to a factor variable in both our training and 
# our testing sets.

# Install randomForest package
install.packages("randomForest")
library(randomForest)
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)
stevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst ,data = Train , nodesize = 25 , ntree = 200)
predictForest = predict(stevensForest, newdata = Test)
table(Test$Reverse, predictForest)

# predictForest
#      0  1
#   0 41 36
#   1 19 74

# (41+74) / (41+74+36+19) = 0.6764706

# Keep in mind that Random Forests has a random component. You may have gotten a different 
# confusion matrix than me because there's a random component to this method.

# being able to understand trust, and explaining a machine learning model, is a highly valuable, 
# if not the most important aspectof a high impact analytics engagement.

# In CART, the value of minbucket can affect the model's out-of-sample accuracy.how should we 
# set this parameter value?


# Cross validation for better minbucket

# So far, we've used the parameter minbucket to limit our tree in R. When we use cross validation 
# in R, we'll use a parameter called cp instead. This is the complexity parameter. It's like 
# Adjusted R-squared for linear regression, and AIC for logistic regression, in that it measures 
# the trade-off between model complexity and accuracyon the training set. A smaller cp value leads 
# to a bigger tree, so a smaller cp value might over-fit the model to the training set. But a cp 
# value that's too large might build a model that's too simple.

install.packages("caret")
install.packages("e1071")
library(e1071)
library(caret)

numFolds = trainControl(method = "cv" , number = 10)
# method = "cv", for cross validation, and then number = 10, for 10 folds.

# Then we need to pick the possible values for our cp. This will define our cp parameters to 
# numbers from 0.01 to 0.5, in increments of 0.01.
cpGrid = expand.grid(.cp=seq(0.01, 0.5, 0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst , data = Train, method = "rpart" , trControl = numFolds , tuneGrid = cpGrid)

# cp = 0.19
# This is the cp value we want to use in our CART model. So now let's create a new CART model 
# with this value of cp,instead of the minbucket parameter.
stevensTreeCv = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train , method = "class" , cp = 0.18)
predictCv = predict(stevensTreeCv, newdata = Test, type = "class")
table(Test$Reverse, predictCv)

#    predictCv
#      0  1
#   0 59 18
#   1 29 64

# (59+64)/(59+64+18+29) = 0.7235294

# Cross validation helps us make sure we're selecting a good parameter value, and often this 
# will significantly increase the accuracy.

prp(stevensTreeCv)
