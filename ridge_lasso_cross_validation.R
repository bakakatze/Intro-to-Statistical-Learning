
require(tidyverse)
require(ISLR)      # free datasets

require(leaps) # A package to perform best subset selection (by Residual Sum of Squares, the syntax is the same as lm)

require(glmnet) # A package to do ridge regression and lasso, it automatically standardise the variables

#
#### Different way of standardising a data ####
data = data.frame(x = c(-1,5,2,3,2,-4,1,2,6,3,-6,3,8,-10,9,-2,1, rep(10,10)))

data = data %>%
  mutate(y = x*10) %>%
  mutate(std_x1 = (x - mean(x))/var(x),
         std_x2 = x/sqrt((1/length(x)) * var(x)),
         std_y1 = (y - mean(y))/var(y),
         std_y2 = y/sqrt((1/length(y)) * var(y)))

# they both have the same spread but on a different center
hist(data$x)
plot(data$x, data$std_x1)
plot(data$x, data$std_x2)


#
#### Dimension Reduction (Theory) ####

# 1. Principal Component Regression (unsupervised)
# PCR reduce the number of components by getting a linear combination of components.
# It does not depend on the variable of interest.

# 2. Partial Least Squares (supervised)
# Similar to PCR but uses the response variable as well.
# The weight of a component variable x_i is dependent on a linear regression coefficient of x_i on the outcome Y.
# So, PLS assigns greater weight towards X_i that correlates better with outcome Y.

#### Subset Selection Methods ####


# We use the Hitters data (baseball players' salary)

str(Hitters)

sum(is.na(Hitters$Salary))

# We can remove missing data listwise
Hitters = na.omit(Hitters)

#### Performs Best Subset Selection
regfit.full = regsubsets(Salary ~., Hitters)
summary(regfit.full)

# by default regsubsets() only shows the best 8-variable model. Use nvmax to increase the limit
regfit.full = regsubsets(Salary ~., data = Hitters, nvmax = 19)
reg.summary = summary(regfit.full)

# we can get the R^2
reg.summary$rsq

# the R^2 increases from 32% (1 var) to almots 55% (19 var)

# let's plot them
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq", type = "l")

# maximum adjr2
maxadjr2 = which.max(reg.summary$adjr2)
# draw the maximum
points(maxadjr2, reg.summary$adjr2[maxadjr2], col = "red", cex = 2, pch = 20)

# let's draw the Cp
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
mincp = which.min(reg.summary$cp)
points(mincp, reg.summary$cp[mincp], col = "red", cex = 2, pch = 20)

# let's draw the BIC
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
minbic = which.min(reg.summary$bic)
points(minbic, reg.summary$bic[minbic], col = "red", cex = 2, pch = 20)

# the regsubsets has a built-in plot function
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")
# lowest BIC (-150) is the model with only 6 variables (AtBat, Hits, Walks, CRBI, DivisionW, and PutOuts)

#### Forward and Backward Stepwise Selection
regfit.fwd = regsubsets(Salary ~., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)

regfit.bwd = regsubsets(Salary ~., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)

## Compare Best Subset Selection, Forward, and Backward Stepwise Selection
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)
# they are all different

#### Validation Set and Cross Validation ####

# remove all NA listwise
Hitters = na.omit(Hitters)

# split the data into training and test
set.seed(1)
train = sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test = (!train)

# we perform regsubsets() on the train dataset
regfit.best = regsubsets(Salary ~., data = Hitters[train,], nvmax = 19)

# create a model matrix
test.mat = model.matrix(Salary ~., data = Hitters[test,])

val.errors = rep(NA, 19)
for(i in 1:19){
  coefi = coef(regfit.best, id = i)
  pred = test.mat[, names(coefi)] %*% coefi              # multiply position c(1,1)[left] with c(1,1)[right] repeat for every position and SUM everything
  val.errors[i] = mean((Hitters$Salary[test]-pred)^2)    # Mean Squared of Errors
}

val.errors
which.min(val.errors) # lowest errors at 10 variables
coef(regfit.best, 10)


## write own fuction to predict the regsubset model
predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])     # call the formula
  mat = model.matrix(form, newdata)       # create a model matrix from the newdata
  coefi = coef(object, id = id)           # get the coefficient of a model
  xvars = names(coefi)                    # get the names of the coefficients
  mat[,xvars] %*% coefi                   # multiply the coefficients with the newdata model matrix and sum them up (this is the prediction result)
}


# Get the 10 best variables on the FULL dataset. This is important to maintain consistency.
regfit.best = regsubsets(Salary ~., data = Hitters, nvmax = 19)
coef(regfit.best, 10)


### Now let's do Cross-Validation!!!
k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace = TRUE)                   # label the k=10 cross-validation sets
cv.errors = matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))    # create a matrix of 10 by 19 for the cv.errors

# the loop command for Cross-Validation
for(j in i:k){
  best.fit = regsubsets(Salary ~ ., data = Hitters[folds!=j, ], nvmax = 19)
  
  for(i in 1:19){
    pred = predict(best.fit, Hitters[folds==j,], id = i)
    cv.errors[j,i] = mean( (Hitters$Salary[folds==j]-pred)^2 )
  }
}


mean.cv.errors = apply(cv.errors, 2, mean)
mean.cv.errors

# we see that cross-validation selects an 11-variable model
# we now perform best subset selection on the full dataset to get the 11-variable model
reg.best = regsubsets(Salary ~., data = Hitters, nvmax = 19)
coef(reg.best, 11)

#
#### Setting Up Data Matrix for Ridge Regression and Lasso ####

# remove all NA listwise, as usual
Hitters = na.omit(Hitters)

# The syntax is a bit different so we need to set up x matrix and y outcome list
x = model.matrix(Salary ~ ., data = Hitters)[,-1]   # without the intercept
y = Hitters$Salary

# model.matrix will automatically transform qualitative variables into dummy variables
# This is important because glmnet can only take numerical values

#### Ridge Regression ####

# set up the possible lambda values
grid = 10^seq(10, -2, length = 100)

# use the x and y model matrix we set up in the previous section
# alpha = 1 is for ridge regression
# glmnet automatically standardise the variables
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid)

# let's look at the coefficient at lambda = 11,497.57
ridge.mod$lambda[50]
coef(ridge.mod)[,50] # a lot of low regression values

# compare it with lambda = 705
coef(ridge.mod)[,60]


# we can use the predict() function to find the coefficients for lamdda = 50
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]


## Split train and test data set
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)

y.test = y[test]

# now, we get prediction for the test set using lambda = 4. Note the 'newx' argument for the test dataset
ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred = predict(ridge.mod, s = 4, newx = x[test, ])

# the MSE
mean((ridge.pred - y.test)^2)

# now, we compare them with least squares (note, least squares is lambda = 0)
ridge.pred = predict(ridge.mod, s = 0, newx = x[test, ])

# the MSE
mean((ridge.pred - y.test)^2) # bigger than lambda = 4


## BUT, instead of arbitrarily choosing lambda = 4, let CV do the job
# by default, cv.glmnet() performs 10-fold cross validation. Can change it using 'nfolds' argument.
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)

# best lamda which has the lowest MSE
bestlam = cv.out$lambda.min
bestlam

# the MSE using the CV approach
ridge.pred = predict(ridge.mod , s = bestlam, newx = x[test, ])
mean((ridge.pred - y.test)^2) # improvement from arbitrarily chosen lambda = 4

# finally, we examine the coefficient of the ridge regression for the full data set
out = glmnet(x, y, alpha = 0)

predict(out,  type = "coefficients", s = bestlam)
# final comment: ridge regression does not perform variable selection.

#### Lasso Regression ####

# alpha = 1 to fit lasso regression
lasso.mod = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

# let's perform cross validation
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s = bestlam, newx = x[test, ])
mean((lasso.pred - y.test)^2) # smaller than least squares, similar to ridge regression (CV)

# but, lasso also performs variable selection
out = glmnet(x , y, alpha = 1, lambda = grid)
lasso.coef = predict(out, type = "coefficients", s = bestlam)
lasso.coef # 12 variables have 0 coefficient





