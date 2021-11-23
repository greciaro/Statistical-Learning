#__________________________Ch4 Prob13___________________________________
#____Using Logistic Regression, LDA and KNN to predict wheter a suburb of Boston data set has a crime rate above or below the median______________

#clean the console
cat("\014")

#Clean all variables
rm(list=ls()) 

if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("MASS") ){ install.packages("MASS") }
if( ! require("class") ){ install.packages("class") }

#Set seed for reproducibility
set.seed(0) 

#Save number of observations in the Boston dataset
n = dim(Boston)[1]

#Create a vector of "n" size with values depending if each observation of coumn "crim" is above (1) or bellow(0) the median
Boston$crim_0_or_1 = integer(n)
Boston$crim_0_or_1[ Boston$crim >= median( Boston$crim ) ] = 1

#Deleting the crim column of Boston dataset
Boston$crim = NULL

#Find the columns that are most correlated with the behavior of crim being above or below the median (crim_0_or_1)
list_best_corr = rev(sort( abs(cor(Boston)[,'crim_0_or_1'] )))
print(list_best_corr)
three_best_corr = names(list_best_corr)[2:4]
print(three_best_corr)

#Divide data set, between the trainning and testing data sets
splitting_portion = 3*n/4
inds_train = sample(1:n,splitting_portion)
inds_test = (1:n)[-inds_train]
Boston_train = Boston[inds_train,]
Boston_test = Boston[inds_test,]

#Fit models and calculate test errors

#Logistic Regression
lr_model = glm( crim_0_or_1 ~ nox + rad + dis, data=Boston_train, family=binomial)
prediction = predict( lr_model, newdata=Boston_test, type="response" )
binary_prediction = rep( 0, length(prediction))
binary_prediction[ prediction > 0.5 ] = 1
CM = table( predicted=binary_prediction, truth=Boston_test$crim_0_or_1 )
