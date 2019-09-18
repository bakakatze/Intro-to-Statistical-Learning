
require(tree) # classification and regression tree

require(ISLR)

require(MASS)

require(randomForest) # bagging and random forest

require(gbm) # boosting

#### Tree-Based Methods

#### Classification Trees ####

# let's use the carseats data set. Sales = continuous variable.
# we dichotomize the Sales variable for classification purposes.
attach(Carseats)

High = ifelse(Sales <= 8, "No", "Yes")

# merge them
Carseats = data.frame(Carseats, High)

# use the classification tree
tree.carseats = tree(High ~. -Sales, data = Carseats)
summary(tree.carseats)

# plot
plot(tree.carseats)
text(tree.carseats, pretty = 0)

# look at the terminal nodes
tree.carseats

## But, as usual. We need to compute the test error and not the training error.
set.seed(2)

train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]

High.test = High[-train]

tree.carseats = tree(High ~.-Sales, Carseats, subset = train)
tree.pred = predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
(87+59)/200  # 73% accuracy

## Next, we consider pruning the tree using cross-validation
set.seed(3)

cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass) # prune.misclass uses missclassification error as the pruning method

# plor the error as a function of size and k
par(mfrow = c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b") # k = cost complexity parameter

cv.carseats
# the lowest deviance is at 9 nodes

## we now obtain the nine-node tree on the original tree classification
prune.carseats = prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

# now let's see the prediction on the test dataset
tree.pred = predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
(94+60)/200 # 77%, it improved!

#
#### Regression Tree ####

# we use Boston dataset. it contains infor on Boston area.
# the outcome we want to predict is medv = median value of owner-occipide homes in $1,000
set.seed(1)

train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv ~ ., Boston, subset = train)
summary(tree.boston)
# only 3 variables have been used: 
# lstat = % lower status of the population
# rm = average # of rooms per dwelling
# dis = weighted distaces to five Boston employment centres

plot(tree.boston)
text(tree.boston, pretty = 0)

# Now we will see if pruning improve performance
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b") # cv will select the most complex tree based on this plot

# let's make the prediction using the unpruned tree
yhat = predict(tree.boston, newdata = Boston[-train, ])
boston.test = Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)

mean((yhat - boston.test)^2) # MSE = 25.04; SD = 5.005

#
#### Bagging and Random Forest ####

set.seed(1)

# we will use the Boston data set again (median house prices in Boston depending on the area)

## BAGGING ##
# We will perform bagging (bagging => special case of random forest with m = p)
bag.boston = randomForest(medv ~., data = Boston, subset = train, mtry = 13, importance = TRUE) #mtry = 13 use all 13 variables
bag.boston

# we will use it to predict the test set
yhat.bag = predict(bag.boston, newdata = Boston[-train,])

plot(yhat.bag, boston.test)
abline(0,1)

mean((yhat.bag-boston.test)^2) # the MSE is 13.31

#
## Random Forest ##
# By default, random forest will use p/3 variables when building a regression tree and sqrt(p) when building a classification tree

# here we use mtry = 6
set.seed(1)
rf.boston = randomForest(medv ~., data = Boston, subset = train, mtry = 6, importance = TRUE)
yhat.rf = predict(rf.boston, newdata = Boston[-train,])

mean((yhat.rf - boston.test)^2) # lower test MSE!!

# see each the importance of each variable
importance(rf.boston)
# 1st column is % increase of MSE if it is left out from the model
# 2nd column = decrease in purity if it is left out from the model

varImpPlot(rf.boston) # plot them

#
#### Boosting ####

set.seed(1)
boost.boston = gbm(medv ~., data = Boston[train, ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(boost.boston)

# partial dependence plot = shows marginal effect of the selected variables (integrate the other variables out)
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")

# let's use it to predict the test data set
yhat.boost = predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test)^2) # even lower than Bagging or Random Forest with 6 variables

#




