# U6-R-Clustering

# will see how to apply clustering techniques to segment images, with the main application 
# being geared towards medical image segmentation.

# Image segmentation is the process of partitioning digital images into regions, or segments, 
# that share the same visual characteristics, such as color, intensity, or texture. The segments 
# should also be meaningful, as in they should correspond to particular surfaces, objects, or 
# even parts of an object.

# the goal of image segmentation is to modify the representation of an image from pixel data 
# into something meaningful to us and easier to analyze.

setwd("C:/Users/Administrator/Desktop/R Projects/unit6/Recitation6")
flower = read.csv("flower.csv", header=FALSE)
str(flower)

# We realize that the way the data is stored does not reflect that this is a matrix of intensity 
# values. Actually, R treats the rows as observations and the columns as variables. Let's try to 
# change the data type to a matrix

# Change the data type to matrix
flowerMatrix = as.matrix(flower)
str(flowerMatrix)

# To perform any type of clustering, we would need to convert the matrix to a vector.

# Turn matrix into a vector
flowerVector = as.vector(flowerMatrix)
str(flowerVector)

# 1. Compute distances
distance = dist(flowerVector, method = "euclidean")

# 2. Hierarchical clustering
clusterIntensity = hclust(distance, method="ward.D")

# Plot the dendrogram
plot(clusterIntensity)


# The vertical lines depict the distance between two nodes or clusters.
# The taller the line, the more dissimilar the clusters are.

# Well, the smaller the number of clusters, the coarser the clustering is.
# But at the same time, having many clusters may be too much of a stretch.
# We should always have this trade-off in mind.

# the distance information between clusters can guide our choice of the number of clusters.
# A good partition belongs to a cut that has a good enough room to move up and down.

# Select 3 clusters
rect.hclust(clusterIntensity, k = 3, border = "red")

# Now let us split the data into these three clusters.
# this function cuts the dendrogram into however many clusters we want.
flowerClusters = cutree(clusterIntensity, k = 3)
flowerClusters

# Find mean intensity values
tapply(flowerVector, flowerClusters, mean)

# To output an image, we can use the image function in R, which takes a matrix as an input.
# But the variable flowerClusters is a vector. So we need to convert it into a matrix

# Plot the image and the clusters
dim(flowerClusters) = c(50,50)
image(flowerClusters, axes = FALSE)

# the input to this function is a sequence of values that goes from 0 to 1, which actually is 
# from black to white. And then we have to also specify its length, and that's specified as 256, 
# because this corresponds to the convention for grayscale.

# Original image
image(flowerMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))


# we will try to segment an MRI brain image of a healthy patient using hierarchical clustering.
healthy = read.csv("healthy.csv", header=FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)

# Plot image
image(healthyMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))

# we see that what we have an MRI imaging of a top section of the brain. And it shows different 
# substances, such as the gray matter, the white matter, and the cerebrospinal fluid. Now let us 
# see if we can isolate these substances via hierarchical clustering.

# Hierarchial clustering
healthyVector = as.vector(healthyMatrix)
distance = dist(healthyVector, method = "euclidean")

# We have an error - why?
str(healthyVector)

# num [1:365636] 0.00427 0.00855 0.01282 0.01282 0.01282 ...
# n = 365636
# n*(n-1)/2
# 66844659430

# Of course R would complain. It's 67 billion values that we're asking R to store in a matrix. 

# The k-means clustering algorithm aims at partitioning the data into k clusters, in a way that 
# each data point belongs to the cluster whose mean is the nearest to it.

# it was impossible for us to use hierarchical clustering because of the high resolution of our 
# image. So we will try to segment the MRI image using the k-means clustering algorithm.

# The first step in k-means clustering involves specifying the number of clusters, k. But how do 
# we select k? setting the number of clusters depends on exactly what you're trying to extract 
# from the image. For the sake of our example, let's set the number of clusters here, k, to five.

# Specify number of clusters
k = 5

# since the k-means is an iterative method that could take very long to converge, we need to set 
# a maximum number of iterations. And we can do this by typing iter.max, and give it, for 
# instance, the value 1,000.

# Run k-means
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)

# List of 9
#  $ cluster     : int [1:365636] 5 5 5 5 5 5 5 5 5 5 ...
#  $ centers     : num [1:5, 1] 0.1166 0.2014 0.4914 0.3299 0.0219
#   ..- attr(*, "dimnames")=List of 2
#   .. ..$ : chr [1:5] "1" "2" "3" "4" ...
#   .. ..$ : NULL
#  $ totss       : num 5775
#  $ withinss    : num [1:5] 65.9 54.4 81.1 47.4 53.3
#  $ tot.withinss: num 302
#  $ betweenss   : num 5473
#  $ size        : int [1:5] 114480 65557 18298 27338 139963
#  $ iter        : int 3
#  $ ifault      : int 0
#  - attr(*, "class")= chr "kmeans"

# Extract clusters
healthyClusters = KMC$cluster

# how can we obtain the mean intensity value within each of our 5 clusters?
# give us the mean intensity value of the second cluster.
KMC$centers[2]

# Plot the image with the clusters
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))

# the rainbow palette, or the function rainbow, takes as an input the number of colors that we 
# want. In this case, the number of colors would correspond to the number of clusters.

image(healthyClusters, axes = FALSE, col=rainbow(k))

# The question now is, can we use the clusters, or the classes, found by our k-means algorithm 
# on the healthy MRI image to identify tumors in another MRI image of a sick patient?

# One common way to select the number of clusters is by using a scree plot, which works for 
# any clustering algorithm. A standard scree plot has the number of clusters on the x-axis, and 
# the sum of the within-cluster sum of squares on the y-axis. The within-cluster sum of squares 
# for a cluster is the sum, across all points in the cluster, of the squared distance between 
# each point and the centroid of the cluster.  We ideally want very small within-cluster sum of 
# squares, since this means that the points are all very close to their centroid. 

# To create the scree plot, the clustering algorithm is run with a range of values for the number 
# of clusters. For each number of clusters, the within-cluster sum of squares can easily be 
# extracted when using k-means clustering. 

SumWithinss = sapply(2:10, function(x) sum(kmeans(healthyVector, centers=x, iter.max=1000)$withinss))
plot(NumClusters, SumWithinss, type="b")

# The plot looks like this (the type="b" argument just told the plot command to give us points 
# and lines)

# To determine the best number of clusters using this plot, we want to look for a bend, or elbow, 
# in the plot. This means that we want to find the number of clusters for which increasing the 
# number of clusters further does not significantly help to reduce the within-cluster sum of 
# squares. For this particular dataset, it looks like 4 or 5 clusters is a good choice. Beyond 5, 
# increasing the number of clusters does not really reduce the within-cluster sum of squares too 
# much.


# we can use these clusters to automatically detect tumors in MRI images of sick patients.

# Apply to a test image
 
tumor = read.csv("tumor.csv", header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

# Now, we will not run the k-means algorithm again on the tumor vector. Instead, we will apply 
# the k-means clustering results that we found using the healthy brain image on the tumor vector. 
# In other words, we treat the healthy vector as training set and the tumor vector as a testing 
# set. for this purpose, we need to convert the information from the clustering algorithm to an 
# object of the class KCCA. And this conversion is needed before we can use the predict function 
# on the test set tumorVector.

# Apply clusters from before to new image, using the flexclust package
install.packages("flexclust")
library(flexclust)

KMC.kcca = as.kcca(KMC, healthyVector)
tumorClusters = predict(KMC.kcca, newdata = tumorVector)

# takes as a first input the original KMC variable that stored all the information from the 
# k-means clustering function, and the second input is the data that we clustered. And in this 
# case, it's the training set, which is the healthyVector.And now, the tumorClusters is a vector 
# that assigns a value 1 through 5 to each of the intensity values in the tumorVector, as 
# predicted by the k-means algorithm.

# Visualize the clusters
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes = FALSE, col=rainbow(k))


# Linear regression is used to predict a continuous outcome. Linear regression is simple and 
# commonly used, and it works on small and large data sets. The downside is that it assumes a 
# linear relationship. If we have a nonlinear relationship, we need to add variables to our 
# analysis.

# Logistic regression is used to predict a categorical outcome. We mainly focused on binary 
# outcomes, like yes or no,In addition to its relative simplicity, logistic regression computes 
# probabilities that can be used to assess the confidence of our prediction. The downside is 
# again similar to that of linear regression.

# In the trees week we learned CART, which is used to predict a categorical outcome, with 
# possibly more than two categories, It can also predict a continuous outcome, such as salary or 
# price.The power of CART lies in the fact that it can handle nonlinear relationships between 
# variables. The tree representation makes it easy to visualize and interpret the results. The 
# downside is that CART may not work very well on small data sets.

# Random forest is also used to predict categorical outcomes or continuous outcomes. Its benefit 
# over CART is that it can improve the prediction accuracy. However, we need to adjust many 
# parameters and it's not as easy to explain as CART

# This week, we learned hierarchical clustering, which is used to find similar groups. An 
# important aspect of clustering data into smaller groups is that we can improve our prediction 
# accuracy by applying our predictive methods, like logistic regression for instance, on each 
# cluster. The drawback though, is that hierarchical clustering is hard to use on large data 
# sets, because of the pairwise distance calculation, An alternative method is k-means clustering, 
# which works well on data sets of any size. However, k-means requires selecting the number of 
# clusters before running the algorithm. This may not be a limitation if we have an intuition of 
# the number of clusters we want to look at.