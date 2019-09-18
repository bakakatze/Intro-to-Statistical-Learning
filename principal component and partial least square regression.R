
require(tidyverse)
require(pls)  # to perform Principal Components Regression and Partial Least Squares Regression

#
#### Principal Components Regression (PCR) ####

set.seed(2)
pcr.fit = pcr(Salary ~ ., data = Hitters, scale = TRUE) # scale = TRUE to standardise each predictor
summary(pcr.fit)
# the first row is the %variance explained
# 1 component = 38.31%, 6 components = 88.63%

# plot the cross-validation MSE
validationplot(pcr.fit, val.type = "MSEP")

## use train dataset and Cross Validation
set.seed(1)
pcr.fit = pcr(Salary ~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

# the lowest is at 7 components
# compute the test MSE
pcr.pred = predict(pcr.fit, x[test, ], ncomp = 7)
mean((pcr.pred - y.test)^2) # is similar to ridge and lasso regression

# the problem with PCR is lack of interpretability of the original predictors

# Finally,  we fit PCR on the full dataset using M = 7
pcr.fit = pcr(y ~ x, scale = TRUE, ncomp = 7)
summary(pcr.fit)

#
#### Partial Least Squares (PLS) ####
set.seed(1)

pls.fit = plsr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)

validationplot(pls.fit) # lowest RMSE at 2 components

# let's look at the test data set
pls.pred = predict(pls.fit, x[test, ], ncomp = 2)
mean((pls.pred - y.test)^2) # slightly higher than ridge, lasso, and PCR

# look at the full data set with 2 components
pls.fit = plsr(Salary ~ ., data = Hitters, scale = TRUE, ncomp = 2)
summary(pls.fit)

# explains 46.40% with only 2 components
# very close to PCR with 7 components

