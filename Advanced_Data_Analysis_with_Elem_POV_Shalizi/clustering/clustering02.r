##########################################################################
#
# Some cluster analysis examples
#
###########################################################################

# Sources for clustering routines in R:
#
# 1. library(mclust) provides Mclust, which does gaussian mixture
#    model based clustering using E-M; selects number of clusters by BIC
#
# 2. kmeans() does basic k-means clustering

# 3. hclust() does hierarchical aggomerative clustering
#    Use plot() to plot the results.  See help(hclust) for more.
#
# 4. library(cluster) provides lots of agglomerative and divisive
#    clustering methods developed by Kaufman and Rousseeuw: agnes,
#    pam, clara, fanny, diana, mona...
#
# Here is a summary of methods (based on MASS, pp. 315-316):
#
# * Agglomerative hierarchical methods (hclust, agnes)
#   - show how to merge smaller clusters into larger ones
#   - main differences are in how to calculate between-cluster
#     distances ("single", "complete" or "average" linkage)
#   - computationally easy & fast
#
# * Optimal partitioning methods (kmeans, pam, clara, fanny)
#   - need to know how many clusters K to produce
#   - need an initial clustering
#   - lots of different criteria to optimize, some model-based
#   - can have distinct 'outlier' group(s)
#
# * Divisive hierarchical methods (diana, mona)
#   - show how to break larger clusters into smaller ones
#   - attractive when a few large clusters are desired
#   - most methods make splits "one variable at a time"
#   - computationally nearly impossible to find optimal solutions
#
# In general, clustering should be used with other data-visualization
# and EDA tools, since clustering often creates too many or too few
# groups to capture the interesting varation in the data.

########################################################################

# 
# The iris data published by Fisher (1936) have been widely used for
# examples in discriminant analysis and cluster analysis. The sepal
# length, sepal width, petal length, and petal width are measured in
# millimeters on fifty iris specimens from each of three species, Iris
# setosa, I. versicolor, and I. virginica.

data(iris)
names(iris)
# [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"  

y <- as.matrix(iris[,-5])[6*(1:25),]   # subsample to make the graphs
rownames(y) <- iris$Species[6*(1:25)]  # pretty

# "default" plot orientation

par(mfrow=c(2,3))

plot(hclust(dist(y),method="single"))
plot(hclust(dist(y),method="complete"))
plot(hclust(dist(y),method="average"))

plot(hclust(dist(y)^2,method="single"))
plot(hclust(dist(y)^2,method="complete"))
plot(hclust(dist(y)^2,method="average"))

# plotting horizontally instead

par(mfrow=c(2,3))

plot(as.dendrogram(hclust(dist(y),method="single")),horiz=T,
     main="dist(y)\nSingle")
plot(as.dendrogram(hclust(dist(y),method="complete")),horiz=T,
     main="dist(y)\nComplete")
plot(as.dendrogram(hclust(dist(y),method="average")),horiz=T,
     main="dist(y)\nAverage")

plot(as.dendrogram(hclust(dist(y)^2,method="single")),horiz=T,
     main="dist(y)^2\nSingle")
plot(as.dendrogram(hclust(dist(y)^2,method="complete")),horiz=T,
     main="dist(y)^2\nComplete")
plot(as.dendrogram(hclust(dist(y)^2,method="average")),horiz=T,
     main="dist(y)^2\nAverage")

#####################################################

# try 'agnes' from library(cluster) -- should be like 'hclust'

library(cluster)

par(mfrow=c(2,3))

plot(hclust(dist(y),method="single"))
plot(hclust(dist(y),method="complete"))
plot(hclust(dist(y),method="average"))

plot(agnes(dist(y),method="single"),which.plots=2)
plot(agnes(dist(y),method="complete"),which.plots=2)
plot(agnes(dist(y),method="average"),which.plots=2)

# I think hclust and agnes are producing exactly the same results,
# only the orientation of some subtrees changes...

#####################################################

# let's look at how good some of these clustering methods
# are, *as classifiers*. 

y <- as.matrix(iris[,1:4])             # same as as.matrix(iris[,-5])
rownames(y) <- iris$Species

hclust.avg <- hclust(dist(y),method="average")

par(mfrow=c(1,1))
plot(hclust.avg)

gps <- c("setosa","versicolor","virginica")[cutree(hclust.avg,3)]

table(true=rownames(y),pred=gps)  # make a "Confusion matrix"

# try it with agnes

agnes.avg <- agnes(dist(y),method="average")
gps <- c("setosa","versicolor","virginica")[cutree(agnes.avg,3)]
table(true=rownames(y),pred=gps)  # make a "Confusion matrix"

# try it with mclust...

library(mclust)

mclust.best <- Mclust(y,G=3)
gps <- c("setosa","versicolor","virginica")[mclust.best$classification]
table(true=rownames(y),pred=gps)  # make a "Confusion matrix"

#############################################################

# try some "optimal" methods (like k-means...)

kmeans.result <- kmeans(y,3)  # note - not a distance mtx!
                              # always use euclidean dist(y)

gps <- c("setosa","versicolor","virginica")[kmeans.result$cluster]
table(true=rownames(y),pred=gps)  # make a "Confusion matrix"

# Note that kmeans does a random assignment of groups to start with,
# so you get a different answer each time you run it.  You can force
# a nonrandom start by specifying the centers of the clusters:

print(centers <- kmeans.result$centers)

new.result <- kmeans(y,centers)
gps <- c("setosa","versicolor","virginica")[new.result$cluster]
table(true=rownames(y),pred=gps)  # make a "Confusion matrix"


# 'pam' works like k-means, but instead of computing the cluster mean
# as the center of each cluster, pam chooses particular values in the
# data set as centers of each data set (so, k-medoids, instead of
# k-means).

# library(cluster)

pam.result <- pam(y,3)
gps <- c("setosa","versicolor","virginica")[pam.result$clustering]
table(true=rownames(y),pred=gps)  # make a "Confusion matrix"

# by default, pam does euclidean distance, but you can ask pam to
# work with other distances too:

pam.result <- pam(dist(y)^2,3)
gps <- c("setosa","versicolor","virginica")[pam.result$clustering]
table(true=rownames(y),pred=gps)  # make a "Confusion matrix"

pam.result <- pam(daisy(y,metric="manhattan"),3)
gps <- c("setosa","versicolor","virginica")[pam.result$clustering]
table(true=rownames(y),pred=gps)  # make a "Confusion matrix"

#############################################################

# try some divisive methods...

# library(cluster)

diana.result <- diana(dist(y))

print(diana.result)

plot(diana.result)

gps <- c("setosa","versicolor","virginica")[cutree(diana.result,3)]
table(true=rownames(y),pred=gps)  # make a "Confusion matrix"

diana.result <- diana(y,metric="manhattan")

plot(diana.result)

gps <- c("setosa","versicolor","virginica")[cutree(diana.result,3)]
table(true=rownames(y),pred=gps)  # make a "Confusion matrix"

# there is also a divisive method called 'mona' that is intended for
# use when all of the variables are binary (T/F, right/wrong,
# symptom/no-symptom, etc.)

########################################################################

