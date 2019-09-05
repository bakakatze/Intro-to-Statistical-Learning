
require(tidyverse)
require(ISLR) # for free datasets
require(MASS)

# stock market data
str(Smarket)

# plot volume
plot(Smarket$Volume)

#
#### 1. Logistic Regression ####

glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fits)

# prediction
glm.probs = predict(glm.fits, type = "response")
glm.probs

# join them back to the data
market_direction = cbind(Smarket, glm.probs)

# classify them into up or down
market_direction = market_direction %>%
  mutate(prediction = ifelse(glm.probs > 0.5, "Up", "Down"))


# accuracty table
table(market_direction$prediction, market_direction$Direction)

mean(market_direction$prediction == market_direction$Direction)
# correct 52.32% of the time, meh
# Plus, this is the training accuracy and not the test accuracy


## Split the train and test dataset
train = (Smarket$Year < 2005)
Smarket.2005 = Smarket[!train, ]
Direction.2005 = Smarket$Direction[!train]

# let's train them
glm.fits = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fits, Smarket.2005 , type = "response")

glm.pred = enframe(glm.probs, name = NULL) %>%
  mutate(prediction = ifelse(as.list(value) > 0.5, "Up", "Down"))

table(glm.pred$prediction, Direction.2005)
mean(glm.pred$prediction == Direction.2005)
# yuck


# now reduce the number of predictors
glm.fits = glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fits, Smarket.2005, type = "response")

glm.pred = rep("Down", 252)
glm.pred[glm.probs>.5] = "Up"

table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
# 56% overall accuracy, improved but not by much

# PPV = 58% << better when the algorithm predicts Up. But, not sure if this is just a random variation.


#### 2. Linear Discriminant Analysis (LDA) ####

lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit

# plot them
plot(lda.fit)

# let's use the LDA model we just built to predict the test dataset
lda.pred = predict(lda.fit, Smarket.2005)

lda.class = lda.pred$class
table(lda.class, Direction.2005)

mean(lda.class == Direction.2005)
# 55.6% accuracy

## But, we can do even more.
# let's say we want to make decission ONLY if the posterior probability > .6
sum(lda.pred$posterior[, 'Down'] > .6)

# lol... none has the posterior probability > 60%
max(lda.pred$posterior[, 'Down'])
# the max is 52.02%.... sad

#### 3. Quadratic Discriminant Analysis ####

qda.fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit


qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)

mean(qda.class == Direction.2005)
# almost 60% accuracy!
# but this is a small sample of dataset

#### 4. K-Nearest Neighbours ####

require(class) # to do K-nearest neighbours algorithm

# get the Lag1 and Lag2 and split them into train and test
train.X = cbind(market_direction$Lag1, market_direction$Lag2)[train,]
test.X = cbind(market_direction$Lag1, market_direction$Lag2)[!train,]

train.Direction = market_direction$Direction[train]

# let's use the knn()
set.seed(1)

knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)

(83+43)/252 # 50% accuracy using k = 1, LOL!


# let's use k = 3
knn.pred = knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)

mean(knn.pred == Direction.2005) # 53%.. meh

#
## We will apply knn algorithm to Caravan Insurance Data

dim(Caravan)

attach(Caravan)

# Only 6% purchase the Caravan insurance in this dataset
summary(Purchase)

## !! important caveats:
# KNN classifier measure on a larger scale will have larger effect
# $100 difference is larger than 10 years old difference as far as KNN can tell regardless of what outcome you are measuring.

# So, in order to handle this, it's best to scale all variables so that they have a mean of 0 and a standard deviation of 1
standardised.X= scale(Caravan[, -86])

# let's split the observations into a test set (first 1000 obs)

test = 1:1000

train.X= standardised.X[-test, ]
test.X = standardised.X[test, ]

train.Y = Purchase[-test]
test.Y = Purchase[test]

set.seed(1)

knn.pred = knn(train.X, test.X, train.Y, k = 1)

mean(test.Y != knn.pred) 
# error rate of 11.8%
# don't get too excited because the buying insurance rate was 6%, so if you make an algorithm that says "no" all the time
# the error rate will be 6% (which is useless)

table(knn.pred, test.Y)

9/(9+68)
# the positive predictive value is 11.7%
# this is an improvement from random guessing (6%)!!

# K = 3 --> 19%
# K = 5 --> 26.7% !!


## Let's compare it with log regression
glm.fits = glm(Purchase ~ ., data = Caravan, family = binomial, subset = -test)

glm.probs = predict(glm.fits, Caravan[test, ], type = "response")

# let's set the probability cutoff of buying insurance at .25
glm.pred = rep("No", 1000)
glm.pred[glm.probs >.25] = "Yes"

table(glm.pred, test.Y)

11/(22+11)
# 33.33% !!


