
require(e1071)  # support vector machine
require(pROC)   # for drawing ROC Curves
require(ISLR)   # for free data sets (Khan - gene expression data)

#### Support Vector Machines

# page 337

# Support Vector Classifier ----

set.seed(1)

x = matrix(rnorm(20*2), ncol = 2)
y = c(rep(-1, 10), rep(1, 10))

x[y == 1, ] = x[y == 1, ] + 1

plot(x, col = (3-y))

# let's transform it into data frame and call y as factor
dat = data.frame(x = x, y = as.factor(y))

svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE) # smaller cost = more support vectors
plot(svmfit, dat)

# we can see that there are 7 support vectors
svmfit$index

summary(svmfit)

# We can tune the cost parameter. The tune() command will perform 10-fold CV by default.
set.seed(1)
tune.out = tune(svm, y ~ ., data = dat, kernel = "linear", 
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))


summary(tune.out) # dispersion = cross validation error rate
# cost = 0.1 is the best

bestmod = tune.out$best.model
summary(bestmod)

#
## Now we apply this to a test data set
xtest = matrix(rnorm(20*2), ncol = 2)
ytest = sample(c(-1,1), 20, rep = TRUE)
xtest[ytest == 1, ] = xtest[ytest == 1, ] + 1

testdata = data.frame(x = xtest, y = as.factor(ytest))

# predict
ypred = predict(bestmod, testdata)
table(predict = ypred, truth = testdata$y)
# with 0.1 cost, 19 out of 20 observations are correctly classified

# Support Vector Machine ----

# page 363

# Support Vector Classifier uses the linear kernel and is really bad for non-linear relationship.

# Support Vector Machine uses other kernel options: polynomial or radial

set.seed(1)

x = matrix(rnorm(200*2), ncol = 2)
x[1:100, ] = x[1:100, ] + 2
x[101:150, ] = x[101:150, ] - 1
y = c(rep(1, 150), rep(2, 50))
dat = data.frame(x = x, y = as.factor(y))

# This is the dummy data that clearly shows non-linearity
plot(x, col = y)

# split into training and test data set
train = sample(200, 100)
svmfit = svm(y ~., data = dat[train, ], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train, ])

summary(svmfit)

# As usual, to find the optimum cost, better to use cross validation
set.seed(1)
tune.out = tune(svm, y ~ ., data = dat[train,], kernel = "radial",
                ranges = list(cost = c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 1, 2, 3, 4)))

summary(tune.out) 
# best performance at: cost = 10  &  gamma = 0.5


# So, off we got with the tuning parameters
pred = unname(predict(tune.out$best.model, newdata = dat[-train,]))
true = dat[-train, "y"]

table(true, pred)
17/(67+16+17) # 17% are misclassified by SVM

# draw the ROC
roc(as.numeric(pred), as.numeric(true))
plot(roc(as.numeric(pred), as.numeric(true)))

##


# Support Vector Machine with Multiple Classes ----

set.seed(1)

x = rbind(x, matrix(rnorm(50*2), ncol = 2))
y = c(y, rep(0, 50))

x[y==0, 2] = x[y==0, 2] + 2
dat = data.frame(x = x, y = as.factor(y))

# this is what the dummy data looks like
plot(x, col = (y+1))

# do SVM
svmfit = svm(y ~., data = dat, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit, dat)

## You still need to read more what algorithm does this code use. One vs One or One vs All.

#
# SVM on Gene Expression Data ----

# this data set contains expression measurements for 2,308 genes.
# the training and test sets consist of 63 and 20 observations respectively.
str(Khan)


# there is a lot of features with a low number of observations. So, we should use "linear" kernel to avoid complications.
dat = data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
out = svm(y ~., data = dat, kernel = "linear", cost = 10)
summary(out)

table(out$fitted, dat$y)
# there are no training error

dat.te = data.frame(x = Khan$xtest, y = as.factor(Khan$ytest))
pred.te = predict(out, newdata = dat.te)
table(pred = pred.te, truth = dat.te$y) # using cost = 10 results in 2 error


