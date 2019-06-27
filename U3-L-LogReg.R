# U3-L-Logistic Regression


# logistic regression, which is an extension of linear regression, is used to environments 
# where the dependent variable is categorical. In our case, 0 or 1.

# Logistic regression predicts the probability of the outcome variable is being true.
# in this example poor care = 1 and good care = 0
# p(y=0) = 1 - p(y=1)
# The Logistic Response Function is used to produce a number between 0 and 1.
# p(y=1) = 1/(1+e^-(B0 + B1x1 + ... + Bkxk)) = 1/(1+e^-linear Regression)
# The coefficients are selected to predict a high probability for the actual poor care cases, 
# and to predict a low probability for the actual good care cases.

# Another useful way to think about the logistic response function is in terms of Odds. The Odds 
# are the probability of 1 divided by the probability of 0. The Odds are greater than 1 if 1 is 
# more likely, and less than 1 if 0 is more likely. The Odds are equal to 1 if the outcomes are 
# equally likely. 

# odds = p(y=1) / p(y=0)
# logit = log(odds) = B0 + B1x1 + ... + Bkxk

# A positive beta value increases the Logit, which in turn increases the Odds of 1. A negative 
# beta value decreases the Logit, which in turn, decreases the Odds of one.

# The Logit is just log(Odds), and looks like the linear regression equation. So the 
# Logit = -1.5 + 3*1 - 0.5*5 = -1.
# Using the value of the Logit from the previous question, we have that 
# Odds = p(1)/p(0) = e^(-1) = 0.3678794.
# Using the Logistic Response Function, we can compute that 
# P(y = 1) = 1/(1 + e^(-Logit)) = 1/(1 + e^(1)) = 0.2689414.

# you can compute e^x in your R console by typing exp(x)


# Let's build a logistic regression model in R to better predict poor care. when we computed the 
# R-squared for linear regression, we compared our predictions to the baseline method of 
# predicting the average outcome for all data points. In a classification problem, a standard 
# baseline method is to just predict the most frequent outcome for all observations. Since good 
# care is more common than poor care,  in this case, we would predict that all patients are 
# receiving good care. If we did this, we would get 98 out  of the 131 observations correct, or 
# have an accuracy of about 75%. So our baseline model has an accuracy of 75%. This is what 
# we'll try to beat with our logistic regression model.

# Read in dataset
quality = read.csv("quality.csv")
str(quality)

# Table outcome
table(quality$PoorCare)
#  0  1 
# 98 33 

# Baseline accuracy
98/131

# Install and load caTools package
install.packages("caTools")
library(caTools)

# In the future, whenever you want to use a package, you won't need to install it, but you will 
# need to load it. let's use this package to randomly split our data into a training set and 
# testing set. Since sample.split randomly splits your data, it could split it differently for 
# each of us. To make sure that we all get the same split, we'll set our seed. This initializes 
# the random number generator.

# Randomly split data
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
split

# sample.split() function from the caTools package to split data for a classification problem, 
# balancing the positive and negative observations in the training and testing sets. We saw 
# earlier that about 75% of our patients are receiving good care. This function makes sure that 
# in our training set, 75% of our patients are receiving good care and in our testing set 75% of 
# our patients are receiving good care.

# Create training and testing sets
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

table(qualityTrain$PoorCare)
#  0  1 
# 74 25 

table(qualityTest$PoorCare)
#  0  1 
# 24  8

# Logistic Regression Model
qualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain , family = binomial())
summary(qualityLog)

# AIC value. This is a measure of the quality of the model and is like Adjusted R-squared in 
# that it accounts for the number of variables used compared to the number of observations. 
# Unfortunately, it can only be compared between models on the same data set. But it provides a 
# means for model selection. The preferred model is the one with the minimum AIC.


# Make predictions on training set
# type = "response" tells the predict function to give us probabilities.
predictTrain = predict(QualityLog, type="response")

# Let's see if we're predicting higher probabilities for the actual poor care cases as we expect.
# Analyze predictions
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)

# outcome of a logistic regression model is a probability. 
# We can convert the probabilities to predictions using what's called a threshold value, t. If 
# the probability of poor care is greater than this threshold value, we predict poor care. 
# But if the probability of poor care is less than the threshold value, then we predict good 
# care. But what value should we pick for the threshold, t?

# We can compute two outcome measures that help us determine what types of errors we are making. They're called 
# sensitivity and specificity.

# confusion matrix
#                   predicted
#                 0           1
#         0       TN          FP
# actual  
#         1       FN          TP

# sensitivity(TPrate) = TP/(TP + FN)
# specificity(TNrate) = TN/(TN + FP)
# N = number of observations
# overall accuracy = (TN + TP)/N
# overall error rate =(FP + FN)/N
# false negative error rate = FN / (TP + FN)
# false positive error rate = FP / (TN + FP)

# A model with a higher threshold will have a lower sensitivity and a higher specificity.
# A model with a lower threshold will have a higher sensitivity and a lower specificity.


# confusion matrices 

# Some decision-makers often have a preference for one type of error over the other, which 
# should influence the threshold value they pick. If there's no preference between the errors, 
# the right threshold to select is t = 0.5, since it just predicts the most likely outcome.

# This will return TRUE if our prediction is greater than 0.5, which means we want to predict 
# poor care, and it will return FALSE if our prediction is less than 0.5, which means we want to 
# predict good care.
table(qualityTrain$PoorCare, predictTrain > 0.5)
   
#     FALSE TRUE
#   0    70    4
#   1    15   10

# TPrate = 10/25 = 0.4
# TNrate = 70/74 = 0.95

# increasing threshold
table(qualityTrain$PoorCare, predictTrain > 0.7)
    
#     FALSE TRUE
#   0    73    1
#   1    17    8

# with the upper threshold, our sensitivity went down, and our specificity went up.

table(qualityTrain$PoorCare, predictTrain > 0.2)
   
#     FALSE TRUE
#   0    54   20
#   1     9   16

# with the lower threshold, our sensitivity went up, and our specificity went down.


# But which threshold should we pick and how do we decide?

# ROC
# to help us select a threshold. ROC curve, can help you decide which value of the threshold 
# is the best.

# The ROC curve always starts at the point (0, 0). This corresponds to a threshold value of 1. 
# If you have a threshold of 1, you will not catch any poor care cases, or have a sensitivity 
# of 0. But you will correctly label of all the good care cases, meaning you have a false positive 
# rate of 0. The ROC curve always ends at the point (1,1),which corresponds to a threshold value 
# of 0. If you have a threshold of 0, you'll catch all of the poor care cases, or have a 
# sensitivity of 1, but you'll label all of the good care cases as poor care cases too, meaning 
# you have a false positive rate of 1.The threshold decreases as you move from (0,0) to (1,1).

# So which threshold value should you pick? You should select the best threshold for the 
# trade-off you want to make.

install.packages("ROCR")
library(ROCR)
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
# or
plot(ROCRperf, colorize = TRUE)
# or
plot(ROCRperf, colorize = TRUE, print.cutoffs.at= seq(0,1,0.1), text.adj=c(-0.5,0.5))


# The colored through its color says what is the threshold on a specific point of the carve.





# So tests to address that involve checking the correlations of independent variables. If they 
# are excessively high, this would mean that there might be multicollinearity, and you have to 
# potentially revisit the model, as well as whether the signs of the coefficients make sense. Is 
# the coefficient beta positive or negative? If it agrees with intuition, then multicollinearity 
# has not been a problem, but if intuition suggests a different sign, this might be a sign of 
# multicollinearity.

# How do we interpret the results, and how do we understand whether we have a good model or not? 
# For that purpose, let's take a look at what is called Area Under the Curve, or AUC.
# the Area Under the Curve shows an absolute measure of quality of prediction and illustrates how 
# accurate the model is on a more absolute sense.


# AUC


# the AUC is the perecentage of time that our model will classify which is which correctly.
# What is the AUC of this model on the test set?

# Compute the test set predictions in R by running the command:
predictTest = predict(qualityLog, type="response", newdata=qualityTest)

# You can compute the test set AUC by running the following two commands in R:
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc
# [1] 0.7994792

# If you wanted to instead split a data frame data, where the dependent variable is a continuous 
# outcome, you could instead use the sample() function. Here is how to select 70% of observations 
# for the training set (called "train") and 30% of observations for the testing set (called "test"):
spl = sample(1:nrow(data), size=0.7 * nrow(data))
train = data[spl,]
test = data[-spl,]