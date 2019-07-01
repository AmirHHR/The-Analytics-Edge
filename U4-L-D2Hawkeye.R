# U4-L-D2Hawkeye

# Our goal will be to predict the cost bucket the patient fell into in 2009 using a CART model.

# To protect the privacy of patients represented in this publicly available data set, a number of 
# steps are performed to anonymize the data. So we would need to retrain the models we develop in 
# this lecture on de-anonymized data if we wanted to apply our models in the real world.

setwd("C:/Users/Administrator/Desktop/R Projects/unit4/D2Hawkeye")
claims <- read.csv("C:/Users/Administrator/Desktop/R Projects/unit4/D2Hawkeye/claimsData.csv")
View(claims)
str(claims)

# 'data.frame':	458005 obs. of  16 variables:
#  $ age              : int  85 59 67 52 67 68 75 70 67 67 ...
#  $ alzheimers       : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ arthritis        : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ cancer           : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ copd             : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ depression       : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ diabetes         : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ heart.failure    : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ ihd              : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ kidney           : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ osteoporosis     : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ stroke           : int  0 0 0 0 0 0 0 0 0 0 ...
#  $ reimbursement2008: int  0 0 0 0 0 0 0 0 0 0 ...
#  $ bucket2008       : int  1 1 1 1 1 1 1 1 1 1 ...
#  $ reimbursement2009: int  0 0 0 0 0 0 0 0 0 0 ...
#  $ bucket2009       : int  1 1 1 1 1 1 1 1 1 1 ...

# Our independent variables are from 2008, and we will be predicting cost in 2009. 
# Each of these variables will take value 1 if the patient had a diagnosis code for the particular 
# disease and value 0 otherwise. Reimbursement2008 is the total amount of Medicare reimbursements 
# for this patient in 2008. And reimbursement2009 is the total value of all Medicare reimbursements 
# for the patient in 2009. Bucket2008 is the cost bucket the patient fell into in 2008, and 
# bucket2009 is the cost bucket the patient fell into in 2009.
# So the first cost bucket contains patients with costs less than $3,000, the second cost bucket 
# contains patients with costs between $3,000 and $8,000, and so on. 


# percentage of patients in each cost bucket 2009.
table(claims$bucket2009)/nrow(claims)

# Our goal will be to predict the cost bucket the patient fell into in 2009 using a CART model.
library(caTools)
set.seed(88)
spl = sample.split(claims$bucket2009 , SplitRatio = 0.6)
claimsTrain = subset(claims, spl == TRUE)
claimsTest = subset(claims, spl == FALSE)

# What proportion of people in the training set (claimsTrain) had at least one diagnosis code 
# for diabetes?
summary(claimsTrain)
# Mean   :0.3809 
# since diabetes is a binary variable, the mean value of diabetes gives the proportion of people 
# with at least one diagnosis code for diabetes.

# To judge the quality of the analytics models we developed, we compare it with a baseline. And 
# the baseline is to simply predict that the cost in the next "period" (bucket 2009 ) will be 
# the cost in the current period (bucket2008).

# smart baseline method : sum of diagonal divided by nrow(claimsTest)
table(claimsTest$bucket2009, claimsTest$bucket2008)
#       1      2      3      4      5
# 1 110138   7787   3427   1452    174
# 2  16000  10721   4629   2931    559
# 3   7006   4629   2774   1621    360
# 4   2688   1943   1415   1539    352
# 5    293    191    160    309    104

# (110138 + 10721 + 2774 + 1539 + 104)/nrow(claimsTest)
# So the accuracy of the baseline method is 0.68. Now how about the penalty error? Keep in mind 
# that we'll put the actual outcomes on the left, and the predicted outcomes on the top.

# Penalty Matrix
penaltyMatrix = matrix(c(0,1,2,3,4, 2,0,1,2,3, 4,2,0,1,2, 6,4,2,0,1, 8,6,4,2,0), byrow = TRUE, nrow = 5)
# penaltyMatrix
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    0    1    2    3    4
# [2,]    2    0    1    2    3
# [3,]    4    2    0    1    2
# [4,]    6    4    2    0    1
# [5,]    8    6    4    2    0
# So now to compute the penalty error of the baseline method, we can multiply our classification 
# matrix by the penalty matrix.
as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2008))*penaltyMatrix

# penalty error
sum(as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2008))*penaltyMatrix)/nrow(claimsTest)
# So the penalty error for the baseline method is 0.7386055

# our goal will be to create a CART model that has an accuracy higher than 68% and a penalty 
# error lower than 0.74.

# What would the accuracy of this baseline method be on the test set if we used the baseline 
# method of predicting the most frequent outcome for all observations?
# table(claimsTest$bucket2009)
#      1      2      3      4      5 
# 122978  34840  16390   7937   1057 
# 122978/nrow(claimsTest)
# 0.67127

# What would the penalty error of this baseline method be on the test set?
# (sum(as.matrix(table(claimsTest$bucket2009))*penaltyMatrix[,1]))/nrow(claimsTest)
# 1.044301

# we'll build a CART model to predict healthcare cost.
library(rpart)
library(rpart.plot)
claimsTree = rpart(bucket2009 ~ . - reimbursement2009 - bucket2009 , data = claimsTrain , 
method = "class" , cp = 0.00005)
prp(claimsTree)

# Note that even though we have a multi-class classification problem here, we build our tree in 
# the same way as a binary classification problem.
predictTest = predict(claimsTree , newdata = claimsTest , type = "class")
table(claimsTest$bucket2009 , predictTest)

#    predictTest
#          1      2      3      4      5
#   1 114141   8610    124    103      0
#   2  18409  16102    187    142      0
#   3   8027   8146    118     99      0
#   4   3099   4584     53    201      0
#   5    351    657      4     45      0
# (114141 + 16102 + 118 + 201 + 0)/nrow(claimsTest)
# 0.7126669

# penalty error
sum(as.matrix((table(claimsTest$bucket2009 , predictTest))*penaltyMatrix)/nrow(claimsTest))
# 0.7578902

# accuracy from 0.68 reaches to 0.7126669
# penalty error from 0.7386055 reaches to 0.7578902

# So while we increased the accuracy, the penalty error also went up. Why? By default, rpart will 
# try to maximize the overall accuracy, and every type of error is seen as having a penalty of 
# one. Our CART model predicts 3, 4, and 5 so rarely because there are very few observations in 
# these classes. So we don't really expect this model to do better on the penalty error than the 
# baseline method. So how can we fix this? The rpart function allows us to specify a parameter 
# called loss. This is the penalty matrix we want to use when building our model.

# By sing this method, We'll probably get a lower overall accuracy with this new model.But 
# hopefully, the penalty error will be much lower too.

claimsTree = rpart(bucket2009 ~ . - reimbursement2009 - bucket2009 , data = claimsTrain , 
method = "class" , cp = 0.00005 , parms = list(loss=penaltyMatrix))
predictTest = predict(claimsTree , newdata = claimsTest , type = "class")
table(claimsTest$bucket2009 , predictTest)
  #  predictTest
  #       1     2     3     4     5
  # 1 94310 25295  3087   286     0
  # 2  7176 18942  8079   643     0
  # 3  3590  7706  4692   401     1
  # 4  1304  3193  2803   636     1
  # 5   135   356   408   156     2
# (94310 + 18942 + 4692 + 636 + 2)/nrow(claimsTest)
# 0.6472746
sum(as.matrix((table(claimsTest$bucket2009 , predictTest))*penaltyMatrix)/nrow(claimsTest))
# 0.6418161

# accuracy from 0.68 (baseline) reaches to 0.7126669 (rpart) and then reaches to 0.6472746(parms)
# penalty error from 0.7386055 (baseline) reaches to 0.7578902 (rpart) and then parm reaches 
# to 0.6418161
