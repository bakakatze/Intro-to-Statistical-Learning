
require(tidyverse)


# load diabetes data:
# 768 samples, 8 quantitative variables, outcome = with or without sign of diabetes

dm = read.csv("diabetes.csv", header = TRUE)


# princomp function to do PCA
# select the explanatory variables only
# The 'cor' argument is for scaling. Need to learn more when it needs scaling.
pca = princomp(dm %>% select(-Outcome), cor = T)


# to get the Principal component scores
pca$scores

# let's get the 1st and 2nd component
pc1 = -1 * pca$scores[,1]
pc2 = -1 * pca$scores[,2]

plot(pc1, pc2, xlim = c(-6, 6), ylim = c(-3, 4), type = "n")
points(pc1[dm$Outcome==0], pc2[dm$Outcome==0], cex=0.5, col="blue")
points(pc1[dm$Outcome==1], pc2[dm$Outcome==1], cex=0.5, col="red")
