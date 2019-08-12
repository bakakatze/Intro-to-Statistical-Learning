
require(tidyverse)
require(ISLR)  # to get wage data
require(class)   # to do K-Nearest Neighbour algorithm and LOOCV
require(KODAMA)  # to do 10-fold cross-validation


#
#### Wage Data ####

# The data consists of 3000 observations, of which 2480 are White and the rest are Black, Asian, or Other.
data(Wage)

WageWhite = Wage %>%
  subset(race == "1. White")

set.seed(679)
M = 50


# select the relevant variables
data = WageWhite %>%
  transmute(jobclass = as.integer(jobclass), education = as.integer(education), year = year, age = age)

#
#### K-Nearest Neighbour ####
# split 50:50
train = sample(nrow(WageWhite), nrow(WageWhite)*0.5, replace = FALSE)
test = -train


# subset the train and test data
train.X = data[train,]
test.X = data[test,]

# dichotomise the wage outcome into greater than 100 or less than equal to 100
train.wage = I(WageWhite$wage > 100)[train]

# create empty variables for the loop command
train.err = rep(NA, M)
test.err = rep(NA, M)


# for loop command
for (i in 1:M) {
  knn.pred = knn(train.X, test.X, train.wage, k = i)
  test.err[i] = mean(knn.pred != I(WageWhite$wage[test]>100))
  
  knn.pred = knn(train.X, train.X, train.wage, k = i)
  train.err[i] = mean(knn.pred != I(WageWhite$wage[train]>100))
}

df = data.frame(c(rep("Training", M), rep("Test", M)), rep(seq(1:M), 2), c(train.err, test.err))
colnames(df) = c("Data", "K", "ErrorRate")

# draw the plot
ggplot(data = df, aes(x = K, y = ErrorRate, group = Data, colour = Data)) +
  geom_line() +
  geom_point()+
  ggtitle("Training and Test Split 50:50")

#
#### 10-Fold Cross-Validation ####

# Description of the algorithm:
# 1. split the data into 10 equal parts
# 2. Set 1 fold as the train and 9 folds as the test
# 3. Re-iterate the #1-#2 using different train fold 10x until each of the folds has been used as the train dataset


err = rep(NA, M)

for (i in 1:M) {
  a = knn.cv(data, I(WageWhite$wage > 100), 1:length(I(WageWhite$wage > 100)), k = i)
  err[i] = mean(a != I(WageWhite$wage > 100))
}

df = data.frame(seq(1:M), err)
colnames(df) = c("K", "ErrorRate")

ggplot(data = df, aes(x = K, y = ErrorRate)) +
  geom_line() +
  geom_point() +
  ggtitle("10-Fold Cross-Validation")

#
#### Leave One Out Cross-Validation (LOOCV) ####

# as the title implies
# leave one out, use the rest of the data as train, predict the one that left out
# rinse and repeat

err = rep(NA, M)

for (i in 1:M) {
  a = knn.cv(data, I(WageWhite$wage > 100), k = 1)
  err[i] = mean(a != I(WageWhite$wage > 100))
}

df<-data.frame(seq(1:M),err)
colnames(df)<-c("K","ErrorRate")

ggplot(data=df, aes(x=K, y=ErrorRate)) +
  geom_line() +
  geom_point() +
  ggtitle("Leave One Out Cross-Validation (LOOCV)")