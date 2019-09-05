
## Resampling method - page 175

require(ISLR) # free datasets
require(boot) # cv.glm function & bootstraping

#
#### The Validation Set Approach ####

set.seed(1)

# let's split the data into training dataset
train = sample(392, 196)

# let's fit a linear regression on the train set of "auto" data
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)

attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
# the MSE is 26.14 for the linear regression

# we can test quadratic function
lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2) # 19.82

# test the cubic function
lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2) #19.78

#
#### Leave-One-Out Cross-Validation (LOOCV) ####

# LOOCV can be automatically computed for any glm model
glm.fit = glm(mpg ~ horsepower, data = Auto)
cv.err = cv.glm(Auto, glm.fit)

cv.err$delta
# this is the MSE of the CV

# we can repeat this process for increasingly complex polynomial fits.
cv.error = rep(0, 5)
for(i in 1:5){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}

cv.error
# the quadratic fit has the most improvement and there is no substantial improvement for the higher order polynomials

#### k-fold Cross-Validation ####

set.seed(17)

cv.error.10 = rep(0,10)

for (i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}

cv.error.10

#
#### The Bootstrap ####

# create a function
alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return( (var(Y) - cov (X,Y)) / (var(X) + var(Y) - 2 * cov(X,Y)) )
}

# This function get standard errors of the bootstraps
alpha.fn(Portfolio, 1:100)
# this gets the standard errors of the bootstrap of X and Y in the Portfolio data using all 100 observations


# This is bootstrap with replacement
set.seed(1)

alpha.fn(Portfolio, sample(100, 100, replace = T))

# We can create a loop command to run it 100x
# But, the boot() function automates this approach
boot(Portfolio, alpha.fn, R = 1000) 

# standed error of the original data (the difference between Y and X) = 0.5758
# the bootstrap estimate  = 0.0886

## Estimating Accuracy of a Linear Regression using Bootstrap ##
# let's use the Auto data

boot.fn = function(data, index){
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}

boot.fn(Auto, 1:392)

# Next, we use boot function to compute standard errors of 1000 bootstrap estimates for the intercept and slope terms
boot(Auto, boot.fn, 1000)

# The bootstrap SE is bigger because the SE in the linear model assumes linear assumption but the bootstrap approach does not!





