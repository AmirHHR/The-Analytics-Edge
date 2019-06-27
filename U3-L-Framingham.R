# U3-L-Framingham

# we will build models using the Framingham data to predict and prevent heart disease.
# let's split our data into a training set and a testing set
setwd("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U3/U3-Framingham")
framingham = read.csv("framingham.csv")
str(framingham)

library(caTools)
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
# we'll put 65% of the data in the training set. When you have more data like we do here, you 
# can afford to put less data in the training set and more in the testing set. This will increase 
# our confidence in the ability of the model to extend to new data since we have a larger test 
# set, and still give us enough data in the training set to create our model.

framinghamTrain = subset(framingham , split == TRUE)
framinghamTest = subset(framingham, split == FALSE)

# We'll use a . for using all of the other variables in the data set as independent variables. 
# Be careful doing this with data sets that have identifying variables like a patient ID or name 
# since you wouldn't want to use these as independent variables.

framinghamLog = glm(TenYearCHD ~ . , data = framinghamTrain , family = binomial())
summary(framinghamLog)

# All of the significant variables have positive coefficients, meaning that higher values in 
# these variables contribute to a higher probability of 10-year coronary heart disease.

predictTest = predict(framinghamLog, type = "response" , newdata = framinghamTest)
# type = "response", which gives us probabilities,

# let's use a threshold value of 0.5 to create a confusion matrix.
table(framinghamTest$TenYearCHD, predictTest > 0.5)
   
#     FALSE TRUE
#   0  1069    6
#   1   187   11

# With a threshold of 0.5, we predict an outcome of 1, the true column, very rarely. This means 
# that our model rarely predicts a 10-year CHD risk above 50%.

# What is the accuracy of this model?
# (1069 + 11)/(1069 + 6 + 187 + 11)
# 0.8483896

# We need to compare this to the accuracy of a simple baseline method. The more frequent outcome 
# in this case is 0, the baseline method would always predict 0 or no CHD.
# (1069 + 6)/(1069 + 6 + 187 + 11)
# 0.8444619
# So our model barely beats the baseline in terms of accuracy.

# Let's compute the out-of-sample AUC.
library(ROCR)
ROCRPredict = prediction(predictTest, framinghamTest$TenYearCHD)
as.numeric(performance(ROCRPredict, "auc")@y.values)
# 0.7421095

# some of the significant variables suggest possible interventions to prevent CHD. We saw that 
# more cigarettes per day, higher cholesterol, higher systolic blood pressure, and higher glucose 
# levels all increased risk.

# What we do here is internal validation but For models that will be used on different 
# populations than the one used to create the model, external validation is critical.

# This means that we took the data from one set of patients and split them into a training set 
# and a testing set. While this confirms that our model is good at making predictions for 
# patients in the Framingham Heart Study population, it's unclear if the model generalizes to 
# other populations. The Framingham cohort of patients were white, middle class adults. To be 
# sure that the model extends to other types of patients, we need to test on other populations.

# the Framingham model systematically over-predicts a risk of CHD. The model can be recalibrated 
# for this population by scaling down the predictions.