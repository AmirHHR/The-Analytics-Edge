# U2-As2-Reading Test Scores

setwd("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U2/U2-As2-Reading Test Scores")
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

# what is the average reading test score of males?
tapply(pisaTrain$readingScore,pisaTrain$male,mean)
#        0        1 
# 512.9406 483.5325 

# Linear regression discards observations with missing data, so we will remove all such 
# observations from the training and testing sets. Later in the course, we will learn about 
# imputation, which deals with missing data by filling in missing values with plausible information.
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

# Factor variables are variables that take on a discrete set of values, like the "Region" 
# variable in the WHO dataset from the second lecture of Unit 1. This is an unordered factor 
# because there isn't any natural ordering between the levels. An ordered factor has a natural 
# ordering between the levels (an example would be the classifications "large," "medium," "small").

table(pisaTrain$male)
#    0    1 
# 1204 1210 
table(pisaTrain$grade) # ordered factor
#    8    9   10   11   12 
#    2  188 1730  491    3 
table(pisaTrain$raceeth) # unordered factor
#  American Indian/Alaska Native                                  Asian 
#                             20                                     95 
#                          Black                               Hispanic 
#                            228                                    500 
#             More than one race Native Hawaiian/Other Pacific Islander 
#                             81                                     20 

# To include unordered factors in a linear regression model, we define one level as the 
# "reference level" and add a binary variable for each of the remaining levels. In this way, a 
# factor with n levels is replaced by n-1 binary variables. The reference level is typically 
# selected to be the most frequently occurring level in the dataset.

# As an example, consider the unordered factor variable "color", with levels "red", "green", and 
# "blue". If "green" were the reference level, then we would add binary variables "colorred" and 
# "colorblue" to a linear regression problem. All red examples would have colorred=1 and 
# colorblue=0. All blue examples would have colorred=0 and colorblue=1. All green examples would 
# have colorred=0 and colorblue=0.

# Because the race variable takes on text values, it was loaded as a factor variable when we 
# read in the dataset However, by default R selects the first level alphabetically 
# ("American Indian/Alaska Native") as the reference level of our factor instead of the most 
# common level ("White"). Set the reference level of the train and test factor
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

# We can train the model with:
lmScore = lm(readingScore~., data=pisaTrain)

# What is the training-set root-mean squared error (RMSE) of lmScore?
SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE / nrow(pisaTrain)); RMSE
# [1] 73.36555
sqrt(mean(lmScore$residuals^2))

# Consider two students A and B. They have all variable values the same, except that student A 
# is in grade 11 and student B is in grade 9. What is the predicted reading score of student A 
# minus the predicted reading score of student B?

# The coefficient 29.54 on grade is the difference in reading score between two students who 
# are identical other than having a difference in grade of 1. Because A and B have a difference 
# in grade of 2, the model predicts that student A has a reading score that is 2*29.54 larger.

# What is the meaning of the coefficient associated with variable raceethAsian?

# The only difference between an Asian student and white student with otherwise identical 
# variables is that the former has raceethAsian=1 and the latter has raceethAsian=0. The 
# predicted reading score for these two students will differ by the coefficient on the variable 
# raceethAsian.

# which variables are candidates for removal from the model? (We'll assume that the factor 
# variable raceeth should only be removed if none of its levels are significant.)

# What is the range between the maximum and minimum predicted reading score on the test set?
predTest = predict(lmScore, newdata=pisaTest)
summary(predTest)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   353.2   482.0   524.0   516.7   555.7   637.7

# What is the sum of squared errors (SSE) of lmScore on the testing set?
SSE = sum((predTest-pisaTest$readingScore)^2); SSE
# 5762082
RMSE = sqrt(mean((predTest-pisaTest$readingScore)^2)); RMSE
# 76.29079

# What is the predicted test score used in the baseline model?
baseline = mean(pisaTrain$readingScore)
# 517.9629

# What is the sum of squared errors of the baseline model on the testing set?
SST = sum((baseline-pisaTest$readingScore)^2)
# 7802354

# What is the test-set R-squared value of lmScore?
R2 = 1 - SSE/SST; R2
[1] 0.2614944
