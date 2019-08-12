
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

# page 161


