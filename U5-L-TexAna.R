# Unit5-L-Text Analytics

# we sent thr sentences to Amazon which helped us to grade them through 5 workers.

# how do we build independent variables from the text of a tweet?
# Fully understanding text is difficult, but Bag of Words provides a very simple approach. 
# It just counts the number of times each word appears in the text and uses these counts as 
# the independent variables.

# preprocessing
# convert each word in lowercase
# delete premarks like # or @
# Another preprocessing task we want to do is to remove unhelpful terms. Many words are 
# frequently used but are only meaningful in a sentence. These are called stop words. Examples 
# are the, is, at, and which. It's unlikely that these words will improve the machine learning 
# prediction quality, so we want to remove them to reduce the size of the data.
# Lastly, an important preprocessing step is called stemming. This step is motivated by the 
# desire to represent words with different endings as the same word.

# Our challenge in this lecture is to see if we can correctly classify tweets as being 
# negative, positive, or neither about Apple.

# bag of words that transforms text into independent variables.

Sys.setlocale("LC_ALL", "C")

# You'll always need to add this extra argument when working on a text analytics problem so that 
# the text is read in properly.
setwd("C:/Users/Administrator/Desktop/R Projects/unit5/lesson")
tweets = read.csv("tweets.csv" , stringsAsFactors = FALSE)
View(tweets)
str(tweets)

# We can see that we have 1,181 observations of two variables, the text of the tweet, called 
# Tweet, and the average sentiment score, called Avg for average.We're more interested in 
# being able to detect the tweets with clear negative sentiment,
tweets$Negative = as.factor(tweets$Avg <= -1)

# This will set tweets$Negative equal to true if the average sentiment score is less than or 
# equal to negative 1 and will set tweets$Negative equal to false if the average sentiment score 
# is greater than negative 1.

# We'll need to convert our tweets to a corpus for pre-processing.
install.packages("tm")
install.packages("SnowballC")

# tm can create a corpus in many different ways, but we'll create it from the tweet column of 
# our data frame using two functions, Corpus and VectorSource.
library(tm)
library(SnowballC)

# create corpus
corpus = VCorpus(VectorSource(tweets$Tweet)) 

# Look at corpus
corpus
corpus[[1]]$content

# we're ready to start pre-processing our data.
# Let's try it out by changing all of the text in our tweets to lowercase.
corpus = tm_map(corpus, content_transformer(tolower))
corpus[[1]]$content

# Remove punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]$content

# Look at stop words 
stopwords("english")[1:10]

# We'll remove all of these English stop words, but we'll also remove the word "apple" since all 
# of these tweets have the word "apple" and it probably won't be very useful in our prediction 
# problem.

# Remove stopwords and apple
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]$content

# Stem document 
corpus = tm_map(corpus, stemDocument)
corpus[[1]]$content

# The tm package provides a function called DocumentTermMatrix that generates a matrix where the 
# rows correspond to documents, in our case tweets, and the columns correspond to words in those 
# tweets. The values in the matrix are the number of times that word appears in each document.
frequencies = DocumentTermMatrix(corpus)
frequencies

# Look at matrix 
# inspect(frequencies[1000:1005,505:515])
# <<DocumentTermMatrix (documents: 6, terms: 11)>>
# Non-/sparse entries: 1/65
# Sparsity           : 98%
# Maximal term length: 9
# Weighting          : term frequency (tf)
# Sample             :
#       Terms
# Docs   cheapen cheaper check cheep cheer cheerio cherylcol chief chiiiiqu child
#   1000       0       0     0     0     0       0         0     0        0     0
#   1001       0       0     0     0     0       0         0     0        0     0
#   1002       0       0     0     0     0       0         0     0        0     0
#   1003       0       0     0     0     0       0         0     0        0     0
#   1004       0       0     0     0     0       0         0     0        0     0
#   1005       0       0     0     0     1       0         0     0        0     0


# This data is what we call sparse. This means that there are many zeros in our matrix.

# Check for sparsity
findFreqTerms(frequencies, lowfreq=20) 

# We see here 56 different words. So out of the 3,289 words in our matrix, only 56 words appear 
# at least 20 times in our tweets. This means that we probably have a lot of terms that will be
# pretty useless for our prediction model. The number of terms is an issue for two main reasons. 
# One is computational. More terms means more independent variables, which usually means it takes 
# longer to build our models. The other is in building models, as we mentioned before, the ratio 
# of independent variables to observations will affect how good the model will generalize. So 
# let's remove some terms that don't appear very often.

# Remove sparse terms
sparse = removeSparseTerms(frequencies, 0.995)
# The sparsity threshold works as follows. If we say 0.98, this means to only keep terms that 
# appear in 2% or more of the tweets.

# If we say 0.995, that means to only keep terms that appear in 0.5% or more of the tweets, 
# about six or more tweets.

sparse
# <<DocumentTermMatrix (documents: 1181, terms: 309)>>
# Non-/sparse entries: 4669/360260
# Sparsity           : 99%
# Maximal term length: 20
# Weighting          : term frequency (tf)

# Now let's convert the sparse matrix into a data frame that we'll be able to use for our 
# predictive models.



# Convert to a data frame
tweetsSparse = as.data.frame(as.matrix(sparse))

# Since R struggles with variable names that start with a number, and we probably have some words 
# here that start with a number, let's run the make.names function to make sure all of our words 
# are appropriate variable names.

# Make all variable names R-friendly
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# This will just convert our variable names to make sure they're all appropriate names before we 
# build our predictive models. You should do this each time you've built a data frame using text 
# analytics.

# Now let's add our dependent variable to this data set.

# Add dependent variable
tweetsSparse$Negative = tweets$Negative

# Split the data
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)

# Build a CART model
library(rpart)
library(rpart.plot)
tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART)

# We're just using the default parameter settings so we won't add anything for minbucket or cp.

# Our tree says that if the word "freak" is in the tweet, then predict TRUE, or negative 
# sentiment. If the word "freak" is not in the tweet, but the word "hate" is, again predict 
# TRUE. If neither of these two words are in the tweet, but the word "wtf" is, also predict 
# TRUE, or negative sentiment. If none of these three words are in the tweet, then predict 
# FALSE, or non-negative sentiment.

# Evaluate the performance of the model
predictCART = predict(tweetCART, newdata=testSparse, type="class")
table(testSparse$Negative, predictCART)
# (294+18)/(294+6+37+18) = 0.8788732

# Baseline accuracy that always predicts non-negative 
table(testSparse$Negative)
# 300/(300+55) = 0.8450704


# Random forest model

library(randomForest)
set.seed(123)

# We'll again use the default parameter settings.
tweetRF = randomForest(Negative ~ ., data=trainSparse)
predictRF = predict(tweetRF, newdata=testSparse)
table(testSparse$Negative, predictRF)
# (293+21)/(293+7+34+21) = 0.884507

# This is a little better than our CART model, but due to the interpretability of our CART 
# model, I'd probably prefer it over the random forest model. If you were to use cross-validation 
# to pick the cp parameter for the CART model, the accuracy would increase to about the same as 
# the random forest model.

# The random forest model takes significantly longer to build than the CART model. We've seen 
# this before when building CART and random forest models, but in this case, the difference is 
# particularly drastic. This is because we have so many independent variables, about 300 
# different words. So far in this course, we haven't seen data sets with this many independent 
# variables. So keep in mind that for text analytics problems, building a random forest model 
# will take significantly longer than building a CART model.

# So by using a bag-of-words approach and these models, we can reasonably predict sentiment even 
# with a relatively small data set of tweets.

# Let's see how well logistic regression does. Build a logistic regression model (using the 
# training set) to predict "Negative" using all of the independent variables.
tweetLog = glm(Negative ~ ., data=trainSparse, family="binomial")
predictLog = predict(tweetLog, newdata=testSparse, type="response")
table(testSparse$Negative, predictLog > 0.5)
# The accuracy is (254+37)/(254+46+18+37) = 0.8197183, which is worse than the baseline. If you 
# were to compute the accuracy on the training set instead, you would see that the model does 
# really well on the training set - this is an example of over-fitting. The model fits the 
# training set really well, but does not perform well on the test set. A logistic regression 
# model with a large number of variables is particularly at risk for overfitting.
predictLogTrain = predict(tweetLog, type="response")
table(trainSparse$Negative, predictLogTrain > 0.5)     
#         FALSE TRUE
#   FALSE   679   20
#   TRUE     15  112
# > (679+112)/(679+112+20+15)
# [1] 0.9576271