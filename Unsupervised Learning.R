
require(ISLR) # collection of data sets

## Unsupervised Learning

# page 373

# Principal Component Analysis ----

# we will use the USArrest data set
# It contains crime statistics and number of population of all the states in the America
states = row.names(USArrests)
states

apply(USArrests, 2, mean)
apply(USArrests, 2, var) # Assault has the largest variance. If we do not standardize this variable, it will drive the the principal components


# we can perform principal component analysis using prcomp(). By default it will center all variables to the mean.
pr.out = prcomp(USArrests, scale = TRUE) # do not forget to scale the variables!!!! so that all variables have SD = 1

# the center and scale matrix will show the mean and the SD used to standardized the variables
pr.out$center
pr.out$scale

# the rotation matrix provides the principal component loadings
pr.out$rotation

# we can plot the first two PC:
biplot(pr.out, scale = 0)

# SD and variance of each PC
pr.out$sdev
pr.var = pr.out$sdev^2
pr.var

# proportion variance explained
pve = pr.var/sum(pr.var)
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = "b")

#
# K-Means Clustering ----

set.seed(2)
x = matrix(rnorm(50*2), ncol = 2)
x[1:25, 1] = x[1:25, 1] + 3
x[1:25, 2] = x[1:25, 2] - 4

# we now perform k-means clustering with K = 2
km.out = kmeans(x, 2, nstart = 20)

km.out$cluster

# plot them
plot(x, col = (km.out$cluster+1), main = "K-Means Clustering (K=2)", xlab = "", ylab = "", pch = 20, cex = 2)

# the nstart argument will assign multiple random starting point
# this is important to avoid getting into undersirable local maxima.
# we can test low and high nstart and compare them:
set.seed(4)
km.out = kmeans(x, 3, nstart = 1)
km.out$tot.withinss # this is the within cluster Sum of Squares from the centroid

km.out = kmeans(x, 3, nstart = 20)
km.out$tot.withinss # lower Sum of Squares compared with the model with nstart = 1

#
# Hierachical Clustering ----

# Hierachical clustering can take different linkage methods:
# 1. Complete = take the highest dissimilarity measure within a cluster to do the 2nd stage of clustering.
# 2. Average = take the average dissimilarity measure within a cluster
# 3. Single = take the smallest dissimilarity measure within a cluster
# 4. Centroid = take the centroid dissimilarity measure within a cluster

# The dist() function uses Euclidean distance for the dissimilarity measure
hc.complete = hclust(dist(x), method = "complete")
hc.average = hclust(dist(x), method = "average")
hc.single = hclust(dist(x), method = "single")

par(mfrow = c(1,3))
plot(hc.complete ,main =" Complete Linkage ", xlab="", sub ="", cex =.9)
plot(hc.average , main =" Average Linkage ", xlab="", sub ="", cex =.9)
plot(hc.single , main=" Single Linkage ", xlab="", sub ="", cex =.9)

# we can use cutree() function to cut the dendogram into a specified amount of leaves
cutree(hc.complete, 2)

# Now, we try scaling our variables
xsc = scale(x)
plot(hclust(dist(xsc), method = "complete"), main = "Hierarchical Clustering with Scaled Features")

# We can use as.dist() function to use correlation-based distance instead of Euclidean
# But, it will only make sense if you have more than 2 variables
x = matrix(rnorm(30*3), ncol = 3)
dd = as.dist(1 - cor(t(x)))
plot(hclust (dd, method = "complete"), main= "Complete Linkage with Correlation-Based Distance", xlab="", sub ="")

#
# NCI60 Data Example ----

# NCI60 microarray data which consists of 6,830 gene expression measurements on 64 cancer cell lines.
nci.labs = NCI60$labs
nci.data = NCI60$data

# Each cell line is labelled with a cancer type but we do not use it just yet because we are going to perform unsupervised learning.
# We will use the cancer type label to validate our model.

dim(nci.data)

# cancer labels
table(nci.labs)

#
## Principal Component Analysis (PCA) on the NCI60 ====
pr.out = prcomp(nci.data, scale = TRUE)

# function to add rainbow colours
Cols = function(vec){
  cols = rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

# plot them
par(mfrow =c(1,2))
plot(pr.out$x[ ,c(1,2)], col = Cols(nci.labs), pch =19, xlab ="Z1", ylab="Z2")
plot(pr.out$x[ ,c(1,3)], col = Cols(nci.labs), pch =19, xlab ="Z1", ylab="Z3")


# look at the PCA summary
summary(pr.out)

# draw the scree plot
pve = 100*(pr.out$sdev^2/sum(pr.out$sdev^2)) # proportion of variance explained
par(mfrow = c(1,2))
plot(pve, type = "o", ylab = "PVE", xlab = "Principal Component", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE", xlab = "Principal Component", col = "brown3")

#
## Heirarchical Clustering the Observations of the NCI60 ====

sd.data = scale(nci.data)

par(mfrow = c(1,3))

data.dist = dist(sd.data) # Euclidean distance

# plot the dendograms
plot(hclust(data.dist), labels = nci.labs, main = "Complete Linkage", xlab="", sub="", ylab="")
plot(hclust (data.dist , method ="average"), labels =nci.labs , main=" Average Linkage ", xlab ="", sub ="", ylab ="")
plot(hclust (data.dist , method ="single"), labels =nci.labs , main=" Single Linkage ", xlab="", sub ="", ylab ="")

# let's cut the dendorgram (Complete Linkage) at 4
hc.out = hclust(dist(sd.data))
hc.clusters = cutree(hc.out, 4)
table(hc.clusters, nci.labs)

par(mfrow = c(1,1))
plot(hc.out, labels = nci.labs)
abline(h=139, col = "red") # the line where the dendogram is split into 4 clusters

#
## K-Means Clustering on the NCI60 ====

set.seed(2)
km.out = kmeans(sd.data, 4, nstart = 20)
km.clusters = km.out$cluster
table(km.clusters, hc.clusters)

#
## Hierarchical Clustering of the PC of the NCI60 ====

# get the first 5 PCs and do hierarchical clustering on them
hc.out = hclust(dist(pr.out$x[, 1:5]))
plot(hc.out , labels =nci.labs , main= "Hier. Clust. on First Five Score Vectors")
table(cutree (hc.out ,4) , nci.labs)

