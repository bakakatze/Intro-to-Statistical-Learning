
require(tidyverse)
require(ISLR)      # free datasets

require(leaps) # A package to perform best subset selection (by Residual Sum of Squares, the syntax is the same as lm)

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
  mat = model.matrix(form, newdata)       # 
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[,xvars] %*% coefi
}

# page 249


