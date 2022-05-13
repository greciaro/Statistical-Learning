





f( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("glmnet") ){ install.packages("glmnet") }
if( ! require("pls") ){ install.packages("pls") }
install.packages("ISLR")
install.packages("glmnet")
install.packages("pls")
library(MASS)
library(leaps)
library(glmnet)
library(pls)
library(caTools)
library(tidyverse)
library(caret)


set.seed(0)

















#STEP 1: SAVE THE DIMENSSIONS OF YOUR DATASET

# CALL n THE NUMBER OF OBSERVATIONS
n = dim(Boston)[1]

# CALL p THE NUMBER OF PARAMETERS
p = dim(Boston)[2]


















# STEP 2: SPLIT YOUR OBSERVATTIONS IN TWO GROUPS:
#         ONE GROUP CALLED "train" TO TRAIN YOUR MODELS
#         ANOTHER GROUP CALLED "test" TO TEST YOUR DATASET

# Best subset selection using cross validation with 10 folds.

predict.regsubsets <- function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars] %*% coefi
}
set.seed(1)
k = 10
folds = sample(1:k, nrow(Boston), replace=TRUE)
cv.errors = matrix(NA,k,13, dimnames = list(NULL, paste(1:13)))
for (j in 1:k) {
  best.fit = regsubsets(crim ~ ., data = Boston[folds != j, ], nvmax = 13)
  for (i in 1:13) {
    pred = predict(best.fit, Boston[folds == j, ], id = i)
    cv.errors[j, i] = mean((Boston$crim[folds == j] - pred)^2)
  }
}
mean.cv.errors = apply(cv.errors, 2, mean)
plot(1:13, mean.cv.errors, xlab = "Number of variables", ylab = "CV error", main= "Best subset selection", pch = 1, type = "b")

#CV error is lowest for model with 9 variables. CV Error = `r mean.cv.errors[9]`.
















#STEP3: APPLIED ALL MODELS SEEN IN CHAPTER 6


# The full linear model:
# 
m = lm( crim ~ ., data=Boston_train )

Y_hat = predict( m, newdata=Boston_test )
MSE = mean( ( Boston_test$crim - Y_hat )^2 )
print( sprintf( "Linear model test MSE= %10.3f", MSE ) ) 


# Ridge regression: 
#
Y = Boston_train$crim 
MM = model.matrix( crim ~ ., data=Boston_train )
cv.out = cv.glmnet( MM, Y, alpha=0 )
plot( cv.out ) 
bestlam = cv.out$lambda.1se
print( "ridge regression CV best value of lambda (one standard error)" )
print( bestlam )

ridge.mod = glmnet( MM, Y, alpha=0 )

Y_hat = predict( ridge.mod, s=bestlam, newx=model.matrix( crim ~ ., data=Boston_test ) )
MSE =mean( ( Boston_test$crim - Y_hat )^2 ) 
print( sprintf( "Ridge regression test MSE= %10.3f", MSE ) ) 


# The Lasso: 
#
cv.out = cv.glmnet( MM, Y, alpha=1 )
plot( cv.out ) 
bestlam = cv.out$lambda.1se
#print( "lasso CV best value of lambda (one standard error)" )
#print( bestlam )

lasso.mod = glmnet( MM, Y, alpha=1 )

Y_hat = predict( lasso.mod, s=bestlam, newx=model.matrix( crim ~ ., data=Boston_test ) )
MSE =mean( ( Boston_test$crim - Y_hat )^2 )
print( sprintf( "Lasso regression test MSE= %10.3f", MSE ) )
print( "lasso coefficients" )
print( predict( lasso.mod, type="coefficients", s=bestlam ) )

# Principle Component Regression:
#
pcr.mod = pcr( crim ~ ., data=Boston_train, scale=TRUE, validation="CV" )

# Use this to select the number of components to include ... looks like CV suggests
# we should use 3 predictors
# 
validationplot( pcr.mod, val.type="MSEP" ) 

ncomp = 3
Y_hat = predict( pcr.mod, Boston_test, ncomp=ncomp )
MSE = mean( ( Boston_test$crim - Y_hat )^2 )
print( sprintf( "PCR (with ncomp= %5d) test MSE= %10.3f", ncomp, MSE ) )

# Paritial Least Squares: 
#
pls.mod = plsr( crim ~ ., data=Boston_train, scale=TRUE, validation="CV" )

# Use this to select the number of components to include ... looks like CV suggests
# the best is to use 5 predictors
# 
validationplot( pls.mod, val.type="MSEP" ) 

ncomp=5
Y_hat = predict( pls.mod, Boston_test, ncomp=ncomp )
MSE = mean( ( Boston_test$crim - Y_hat )^2 ) 
print( sprintf( "PLS (with ncomp= %5d) test MSE= %10.3f", ncomp, MSE ) )

