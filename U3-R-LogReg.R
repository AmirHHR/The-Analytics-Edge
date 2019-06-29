# Unit 3 - Recitation 3 - Logestic Regression

# election forecasting, which is the art and science of predicting the winner of an election 
# before any votes are actually cast using polling data from likely voters.

# 1. omitting the NA 
# 2. producing Train and Test
# 3. cor of the variables
# 4. model
# 5. predict on the Train
# 6. table

# the dependent variable, Republican, is a binary outcome. It's 1 if the Republican won that 
# state in that particular election year, and a 0 if a Democrat won.

# Read in data
polling = read.csv("PollingData.csv")
str(polling)
table(polling$Year)
summary(polling)

# how we can handle this missing data in our data frame?
# So for our multiple imputation to be useful, we have to be able to find out the values of our 
# missing variables without using the outcome of Republican. So, what we're going to do here is 
# we're going to limit our data frame to just the four polling related variables before we 
# actually perform multiple imputation.

# fill the missing data values based on the non-missing values for an observation.
install.packages('mice')
library('mice')
simple = Polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)

# now all of the variables have been filled in. So there's no more missing values.
# So the last step in this imputation process is to actually copy the Rasmussen and SurveyUSA 
# variables back into our original polling data frame, which has all the variables for the 
# problem.
Polling$Rasmussen = imputed$Rasmussen
Polling$SurveyUSA = imputed$SurveyUSA
summary(polling)

# Using the Imputed data set
# we're actually going to train on data from the 2004 and 2008 elections, and we're going to 
# test on data from the 2012 presidential election.
Train = subset(Polling, Year == 2004 | Year == 2008)
Test = subset(Polling, Year == 2012)

# we want to understand the prediction of our baseline model against which we want to compare 
# a later logistic regression model.
table(Train$Republican)

#  0  1 
# 47 53 

# our simple baseline model is always going to predict the more common outcome, which is that 
# the Republican is going to win the state. And we see that the simple baseline model will have 
# accuracy of 53% on the training set. Now, unfortunately, this is a pretty weak model. It always 
# predicts Republican, even for a very landslide Democratic state, where the Democrat was polling 
# by 15% or 20% ahead of the Republican. So nobody would really consider this to be a credible 
# model. So we need to think of a smarter baseline model against which we can compare our logistic 
# regression models that we're going to develop later.

# So a reasonable smart baseline would be to just take one of the polls-- in our case, we'll 
# take Rasmussen-- and make a prediction based on who poll said was winning in the state. So for 
# instance, if the Republican is polling ahead, the Rasmussen smart baseline would just pick the 
# Republican to be the winner. If the Democrat was ahead, it would pick the Democrat. And if they 
# were tied, the model would not know which one to select.

# we're going to use a new function called the sign function. And what this function does is, 
# if it's passed a positive number(republican > democrat), it returns the value 1. If it's 
# passed a negative number (republican < democrat), it returns negative 1. And if it's passed 0, 
# it returns 0.
table(sign(Train$Rasmussen))

# -1  0  1 
# 42  2 56 

# what we really want to do is to see the breakdown of how the smart baseline model does, 
# compared to the actual result -- who actually won the state.
table(Train$Republican, sign(Train$Rasmussen))
   
                 # -1  0  1
# Democrat #     0 42  1  4
# Republican #   1  0  1 52

# we have 42 observations where the Rasmussen smart baseline predicted the Democrat would win, 
# and the Democrat actually did win. There were 52 observations where the smart baseline predicted 
# the Republican would win, and the Republican actually did win. Again, there were those two 
# inconclusive observations. And finally, there were four mistakes. There were four times where 
# the smart baseline model predicted that the Republican would win, but actually the Democrat won 
# the state. So as we can see, this model, with four mistakes and two inconclusive results out of 
# the 100 training set observations is doing much, much better than the naive baseline, which 
# simply was always predicting the Republican would win and made 47 mistakes on the same data. So 
# we see that this is a much more reasonable baseline model to carry forward, against which we 
# can compare our logistic regression-based approach.

# cor(Train) produces an error because all of elements should be numerical so we delete the 
# "State" and computes it again
cor(Train)
str(Train)
cor(Train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])

#            Rasmussen SurveyUSA     PropR DiffCount Republican
# Rasmussen  1.0000000 0.9365837 0.8431180 0.5109169  0.7929252
# SurveyUSA  0.9365837 1.0000000 0.8616478 0.5222585  0.8101645
# PropR      0.8431180 0.8616478 1.0000000 0.8273785  0.9484204
# DiffCount  0.5109169 0.5222585 0.8273785 1.0000000  0.8092777
# Republican 0.7929252 0.8101645 0.9484204 0.8092777  1.0000000

# So let's first consider the case where we want to build a logistic regression model with just 
# one variable. So in this case, it stands to reason that the variable we'd want to add would be 
# the one that is most highly correlated with the outcome, Republican.

# Logistic Regression Model
mod1 = glm(Republican ~ PropR , data = Train , family = binomial())
summary(mod1)
# AIC: 19.772

# Training set predictions
pred1 = predict(mod1 , type = "response")
table(Train$Republican , pred1 >= 0.5)
   
#     FALSE TRUE
#   0    45    2--------------> 4 false
#   1     2   51

# TRUE means that we predicted Republican, FALSE means we predicted Democrat.
# So we see that on the training set, this model with one variable as a prediction makes four 
# mistakes, which is just about the same as our smart baseline model. So now, let's see if we 
# can improve on this performance by adding in another variable.

# So if we go back up to our correlations, we're going to be searching, since there's so much 
# multicollinearity, we might be searching for a pair of variables that has a relatively lower 
# correlation with each other, because they might kind of work together to improve the prediction 
# overall of the Republican outcome. If two variables are highly, highly correlated, they're less 
# likely to improve predictions together, since they're so similar in their correlation structure.

# Two-variable model
mod2 = glm(Republican ~ SurveyUSA + DiffCount , data = Train , family = binomial())
summary(mod2)
# AIC: 17.154

pred2 = predict(mod2 , type = "response")
table(Train$Republican , pred2 >= 0.5)
#     FALSE TRUE
#   0    45    2-------------> 3 false
#   1     1   52

# we can see that there are some things that are pluses. For instance, the AIC has a smaller 
# value, which suggests a stronger model. And the estimates have, again, the sign we would 
# expect. So SurveyUSA and DiffCount both have positive coefficients in predicting if the 
# Republican wins the state, which makes sense. But a weakness of this model is that neither 
# of these variables has a significance of a star or better, which means that they are less 
# significant statistically. So there are definitely some strengths and weaknesses between the 
# two-variable and the one-variable model.


# the first model we're going to want to evaluate on the test set is that smart baseline model 
# that basically just took a look at the polling results from the Rasmussen poll and used those 
# to determine who was predicted to win the election.
table(Test$Republican, sign(Test$Rasmussen))
   
#     -1  0  1
#   0 18  2  4
#   1  0  0 21 

TestPrediction = predict(mod2 , newdata = Test , type = "response")
table(Test$Republican, TestPrediction >= 0.5)
   
#     FALSE TRUE
#   0    23    1
#   1     0   21

# let's take a look now at the mistake we made and see if we can understand what's going on.
subset(Test, TestPrediction >= 0.5 & Republican == 0)

#      State Year Rasmussen SurveyUSA DiffCount     PropR Republican
# 24 Florida 2012         2         0         6 0.6666667          0