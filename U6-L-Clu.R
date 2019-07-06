Unit 6 - Lesson - Clustering

# we'll discuss how analytics can be used to make recommendations for movies and for health and
# discuss the method of clustering, which is used to find similarities and patterns in data.

# This technique of using other user's ratings to make predictions is called collaborative 
# filtering. Note that we're not using any information about the movie itself here, just the 
# similarity between users.

# Note that we're not using the ratings of other users at all here, just information about the 
# movie. This technique is called content filtering. There are strengths and weaknesses to both 
# types of recommendation systems. Collaborative filtering can accurately suggest complex items 
# without understanding the nature of the items. It didn't matter at all that our items were 
# movies in the collaborative filtering example. We were just comparing user ratings. However, 
# this requires a lot of data about the user to make accurate recommendations. Also, when there 
# are millions of items, it needs a lot of computing power to compute the user similarities. On 
# the other hand, content filtering requires very little data to get started. But the major
# weakness of content filtering is that it can be limited in scope. You're only recommending 
# similar things to what the user has already liked. So the recommendations are often not 
# surprising or particularly insightful. Netflix actually uses what's called a hybrid 
# recommendation system

# we'll use a method called clustering. Clustering is different from the other analytics 
# methods we've covered so far. It's called an unsupervised learning method. This means that 
# we're just trying to segment the data into similar groups, instead of trying to predict an 
# outcome.

# This is the goal of clustering-- to put each data point into a group with similar values in 
# the data. A clustering algorithm does not predict anything. However, clustering can be used 
# to improve predictive methods. You can cluster the data into similar groups and then build a 
# predictive model for each group. This can often improve the accuracy of predictive methods. 
# But as a warning, be careful not to over-fit your model to the training set. This works best 
# for large data sets. To cluster data points, we need to compute how similar the points are. 
# This is done by computing the distance between points,

# we'll see how we can do content filtering by using a method called clustering.

# The first step in clustering is to define the distance between two data points. The most 
# popular way to compute the distance is what's called Euclidean distance.In addition to Euclidean 
# distance, there are many other popular distance metrics that could be used. One is called 
# Manhattan distance, where the distance is computed to be the sum of the absolute values instead 
# of the sum of squares. Another is called maximum coordinate distance, where we only consider 
# the measurement for which the data points deviate the most.

# how do we compute the distance between groups of points? One way of doing this is by using 
# what's called the minimum distance.This defines the distance between clustersas the distance 
# between the two data points in the clustersthat are closest together. Alternatively, we could 
# use maximum distance. This one computes the distance between the two clusters as the distance 
# between the two pointsthat are the farthest apart. The most common distance metric between 
# clusters is called centroid distance.

# we are computing distances, it's highly influenced by the scale of the variables. To handle 
# this, it's customary to normalize the data first. We can normalize by subtracting the mean of 
# the data and dividing by the standard deviation.


# hierarchical clustering

# In hierarchical clustering, the clusters are formed by each data point starting in its own 
# cluster.
# Then hierarchical clustering combines the two nearest clusters into one cluster. We'll use 
# Euclidean and Centroid distances to decide which two clusters are the closest and the process 
# repeats. So now the final step is to combine these two clusters into one cluster. So at the 
# end of hierarchical clustering, all of our data points are in a single cluster.

# The hierarchical cluster process can be displayed through what's called a dendrogram. The data 
# points are listed along the bottom, and the lines show how the clusters were combined. The 
# height of the lines represents how far apart the clusters were when they were combined. We can 
# use a dendrogram to decide how many clusters we want for our final clustering model. The 
# easiest way to pick the number of clusters you want is to draw a horizontal line across the 
# dendrogram. The number of vertical lines that line crosses is the number of clusters there 
# will be. The farthest this horizontal line can move up and down in the dendrogram without 
# hitting one of the horizontal lines of the dendrogram, the better that choice of the number 
# of clusters is.when picking the number of clusters, you should also consider how many 
# clusters make sense for the particular application you're working with.

# After selecting the number of clusters you want, you should analyze your clusters 
# to see if they're meaningful. This can be done by looking at basic statistics in each cluster, 
# like the mean, maximum, and minimum values in each cluster and each variable. You can also 
# check to see if the clusters have a feature in common that was not used in the clustering, 
# like an outcome variable. This often indicates that your clusters might help improve a 
# predictive model.

# we've seen one example of how to prepare data taken from the internet to work with it in R.

# Video 6

setwd("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U6/lesson")
movies = read.table("movieLens.txt", header=FALSE, sep="|",quote="\"")
str(movies)
# That last argument "\"" just made sure that our text was read in properly.


# Add column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
str(movies)

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates in our dataset
movies = unique(movies)
str(movies)

# we'll use hierarchical clustering to cluster the movies in the Movie Lens data set by genre. 
# After we make our clusters, we'll see how they can be used to make recommendations.

# Compute distances without Title variable
distances = dist(movies[2:20], method = "euclidean")

# Hierarchical clustering
clusterMovies = hclust(distances, method = "ward.D") 

# The ward method cares about the distance between clusters using centroid distance, and also 
# the variance in each of the clusters.

# Plot the dendrogram
plot(clusterMovies)
# it's impossible to read. We'll see later how to assign our clusters to groups so that we can 
# analyze which data points are in which cluster.

# It looks like maybe three or four clusters would be a good choice according to the dendrogram, 
# but let's keep our application in mind, too. We probably want more than two, three, or even 
# four clusters of movies to make recommendations to users. It looks like there's a nice spot 
# down here where there's 10 clusters. This is probably better for our application. We could 
# select even more clusters if we want to have very specific genre groups. If you want a lot of 
# clusters it's hard to pick the right number from the dendrogram. You need to use your 
# understanding of the problem to pick the number of clusters.

# Let's stick with 10 clusters for now, combining what we learned from the dendrogram with our 
# understanding of the problem.

# Assign points to clusters we can label each of the data points according to what cluster it 
# belongs to using the cutree function. we makes 10 clusters:
clusterGroups = cutree(clusterMovies, k = 10)

# Now let's figure out what the clusters are like.
# Let's use the tapply function to compute the percentage of movies in each genre and cluster
tapply(movies$Action, clusterGroups, mean)
#         1         2         3         4         5         6         7         8 
# 0.1784512 0.7839196 0.1238532 0.0000000 0.0000000 0.1015625 0.0000000 0.0000000 
#         9        10 
# 0.0000000 0.0000000 

# It divides our data points into the 10 clusters and then computes the average value of the 
# action variable for each cluster. Remember that the action variable is a binary variable with 
# value 0 or 1. So by computing the average of this variable we're computing the percentage of 
# movies in that cluster that belong in that genre. So we can see here that in cluster 2, about 
# 78% of the movies have the action genre label, whereas in cluster 4 none of the movies are 
# labeled as action movies.


# let's see how these clusters could be used in a recommendation system. 

# Find which cluster Men in Black is in.
subset(movies, Title=="Men in Black (1997)")

# So which cluster did the 257th movie go into?
clusterGroups[257]

# Create a new data set with just the movies from cluster 2
cluster2 = subset(movies, clusterGroups==2)

# Look at the first 10 titles in this cluster
cluster2$Title[1:10]
#  [1] GoldenEye (1995)                              
#  [2] Bad Boys (1995)                               
#  [3] Apollo 13 (1995)                              
#  [4] Net, The (1995)                               
#  [5] Natural Born Killers (1994)                   
#  [6] Outbreak (1995)                               
#  [7] Stargate (1994)                               
#  [8] Fugitive, The (1993)                          
#  [9] Jurassic Park (1993)                          
# [10] Robert A. Heinlein's The Puppet Masters (1994)
# 1664 Levels: 'Til There Was You (1997) ... Zeus and Roxanne (1997)

# So it looks like good movies to recommend to the one who likes "Men In Black", according to 
# our clustering algorithm.



# An Advanced Approach to Finding Cluster Centroids


# In this video, we explain how you can find the cluster centroids by using the function 
# "tapply" for each variable in the dataset. While this approach works and is familiar to us, 
# it can be a little tedious when there are a lot of variables. An alternative approach is to 
# use the colMeans function. With this approach, you only have one command for each cluster 
# instead of one command for each variable. If you run the following command in your R console, 
# you can get all of the column (variable) means for cluster 1:

colMeans(subset(movies[2:20], clusterGroups == 1))

# You can repeat this for each cluster by changing the clusterGroups number. However, if you also 
# have a lot of clusters, this approach is not that much more efficient than just using the 
# tapply function. A more advanced approach uses the "split" and "lapply" functions. The following 
# command will split the data into subsets based on the clusters:

spl = split(movies[2:20], clusterGroups)

# Then you can use spl to access the different clusters, because

spl[[1]]

# is the same as

subset(movies[2:20], clusterGroups == 1)

# so 
colMeans(spl[[1]])

# will output the centroid of cluster 1.

# But an even easier approach uses the lapply function. The following command will output the 
# cluster centroids for all clusters:

lapply(spl, colMeans)

# The lapply function runs the second argument (colMeans) on each element of the first argument 
# (each cluster subset in spl). So instead of using 19 tapply commands, or 10 colMeans commands, 
# we can output our centroids with just two commands: one to define spl, and then the lapply 
# command.

spl = split(movies[2:20], clusterGroups)
lapply(spl, colMeans)

# Recommendation systems build models about users' preferences to personalize the user 
# experience. This helps users find items they might not have searched for. Excellent 
# recommendation systems can make or break these businesses. Clustering algorithms, which are 
# tailored to find similar customers or similar items, form the backbone of many of these 
# recommendation systems.

# Drawing heatmaps with R

library(tidyverse)
library(ztable) 
spl <- split(movies[2:20], clusterGroups)
do.call(rbind, lapply(spl, colMeans)) %>% 
  ztable(caption = "Cluster centroids") %>%
  makeHeatmap()


# Heart attack

# Let us now introduce the idea of clustering. Patients in each bucket may have different 
# characteristics. For this reason, we create clusters for each cost bucket and make predictions 
# for each cluster using the Random Forest algorithm.

# after splitting the dataset into train and test, we perform clustering on them and after that 
# run the random forest algorithm on each clusters.
# So how do we measure performance? After we construct the clusters in the training set, we 
# assign new observations to clusters by proximity to the centroid of each cluster. We measure 
# performance by recording the average performance rate in each cluster.

# The Random Forest algorithm is known for its attractive property of detecting variable 
# interactions and excellent performance as a learning algorithm. Clustering is mostly used in 
# the absence of a target variable to search for relationships among input variables or to 
# organize data into meaningful groups. In this study, although the target variable is well-
# defined as a heart attack or not a heart attack, there are many different trajectories that are 
# associated with the target. There's not one set pattern of health or diagnostic combination 
# that leads a person to heart attack. Instead, we'll show that there are many different dynamic 
# health patterns and time series diagnostic relations preceding a heart attack.


# K-means algorithm

# We first specify the number of clusters k.
# Then we randomly assign each data point to a cluster.
# We then compute the cluster centroids.
# We re-assign each point to the closest cluster centroid.
# We then re-compute the cluster centroids,
# and we repeat steps 4 and 5 until no improvement is made.

# The number of clusters k can be selected from previous knowledge or by simply experimenting. 
# We can strategically select an initial partition of points into clusters if we have some 
# knowledge of the data. We can also run the algorithm several times with different random 
# starting points. If you wanted to find more unusual patterns, you would increase the number 
# of clusters since the clusters would become smaller and more patterns would probably emerge.

