# Unit 5 - Recitation

# detecting the reason for bankrupt through emails
# Load the dataset
emails = read.csv("energy_bids.csv", stringsAsFactors=FALSE)
str(emails)

# Look at emails
emails$email[1]

# we can see that this has broken down our long string into multiple shorter lines that are much 
# easier to read.
strwrap(emails$email[1])
emails$responsive[1]

# Responsive emails
# now let's look at the breakdown of the number of emails that are responsive to our query
table(emails$responsive)


library(tm)

# Create corpus
corpus = VCorpus(VectorSource(emails$email))
corpus[[1]]$content


# Pre-process data
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

# Look at first email
corpus[[1]]$content


# Create matrix
dtm = DocumentTermMatrix(corpus)
dtm

# <<DocumentTermMatrix (documents: 855, terms: 22164)>>
# Non-/sparse entries: 102863/18847357
# Sparsity           : 99%
# Maximal term length: 156
# Weighting          : term frequency (tf)

# we have only 855 emails in the corpus, we have over 22,000 terms that showed up at least 
# once, which is clearly too many variables for the number of observations we have. So we want 
# to remove the terms that don't appear too often in our data set.

# Remove sparse terms
dtm = removeSparseTerms(dtm, 0.97)
dtm

# <<DocumentTermMatrix (documents: 855, terms: 788)>>
# Non-/sparse entries: 51612/622128
# Sparsity           : 92%
# Maximal term length: 19
# Weighting          : term frequency (tf)

# So this data frame is only including right now the frequencies of the words that appeared in 
# at least 3% of the documents,

# Create data frame
labeledTerms = as.data.frame(as.matrix(dtm)) 

# in order to run our text analytics models, we're going to have the outcome variable, which is 
# whether or not each email was responsive. So we need to add in this outcome variable.

# Add in the outcome variable
labeledTerms$responsive = emails$responsive
str(labeledTerms)


# Split the data

library(caTools)
set.seed(144)
spl = sample.split(labeledTerms$responsive, 0.7)
train = subset(labeledTerms, spl == TRUE)
test = subset(labeledTerms, spl == FALSE)

# Build a CART model
library(rpart)
library(rpart.plot)
emailCART = rpart(responsive~., data=train, method="class")
prp(emailCART)

# So we can see at the very top is the word California. If California appears at least twice in 
# an email, we're going to take the right part over here and predict that a document is 
# responsive. It's somewhat unsurprising that California shows up, because we know that Enron 
# had a heavy involvement in the California energy markets.

# Make predictions on the test set
pred = predict(emailCART, newdata=test)
# the comma indicates all columns should be presented
pred[1:10,] 
pred.prob = pred[,2]

# Compute accuracy
table(test$responsive, pred.prob >= 0.5)
# (195+25)/(195+25+17+20) = 0.856

# Baseline model accuracy based on non responsive for all elements
table(test$responsive)
# 215/(215+42) = 0.836

# as in most document retrieval applications, there are uneven costs for different types of 
# errors here. Typically, a human will still have to manually review all of the predicted 
# responsive documents to make sure they are actually responsive. Therefore, if we have a false 
# positive, in which a non-responsive document is labeled as responsive, the mistake translates 
# to a bit of additional work in the manual review process but no further harm, since the manual 
# review process will remove this erroneous result. But on the other hand, if we have a false 
# negative, in which a responsive document is labeled as non-responsive by our model, we will 
# miss the document entirely in our predictive coding process. Therefore, we're going to assign 
# a higher cost to false negatives than to false positives, which makes this a good time to 
# look at other cut-offs on our ROC curve.

# table(test$responsive, pred.prob >= 0.4)  
#     FALSE TRUE
#   0   195   20
#   1    17   25

# table(test$responsive, pred.prob >= 0.3)  
#     FALSE TRUE
#   0   195   20
#   1    17   25

# table(test$responsive, pred.prob >= 0.2)
#     FALSE TRUE
#   0   195   20
#   1    17   25

# table(test$responsive, pred.prob >= 0.1)   
#     FALSE TRUE
#   0   176   39
#   1    12   30

# table(test$responsive, pred.prob >= 0.15)   
#     FALSE TRUE
#   0   176   39
#   1    12   30

# > table(test$responsive, pred.prob >= 0.19)   
#     FALSE TRUE
#   0   195   20
#   1    17   25
# table(test$responsive, pred.prob >= 0.18)
   
#     FALSE TRUE
#   0   190   25
#   1    15   27

# table(test$responsive, pred.prob >= 0.17)   
#     FALSE TRUE
#   0   190   25
#   1    15   27

# table(test$responsive, pred.prob >= 0.16)   
#     FALSE TRUE
#   0   176   39
#   1    12   30

# the ROC curve can understand the performance of our model at different cutoffs.

# ROC curve
library(ROCR)
predROCR = prediction(pred.prob, test$responsive)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)

# in this part of the curve, where we have a true positive rate of around 70%, meaning that 
# we're getting about 70% of all the responsive documents, and a false positive rate of about 
# 20%, meaning that we're making mistakes and accidentally identifying as responsive 20% of the 
# non-responsive documents. Now, since, typically, the vast majority of documents are 
# non-responsive, operating at this cutoff would result, perhaps, in a large decrease in the 
# amount of manual effort needed in the eDiscovery process. And we can see from the blue color 
# of the plot at this particular location that we're looking at a threshold around maybe 0.15 or 
# so, significantly lower than 50%, which is definitely what we would expect since we favor false 
# positives to false negatives.

performance(predROCR, "auc")@y.values
# [1] 0.7936323
# We can see that we have an AUC in the test set of 79.4%, which means that our model can 
# differentiate between a randomly selected responsive and non-responsive document about 80% of 
# the time.