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

#Create a vector of "n" size of values 0 or 1 depending if each observation of coumn "crim" is above or bellow the median
Boston$crim_0_or_1 = integer(n)
Boston$crim_0_or_1[ Boston$crim >= median( Boston$crim ) ] = 1

#Deleting the crim column of Boston dataset
Boston$crim = NULL

#Find the columns that are most correlated with the behavior of crim being above or below the median (crim_0_or_1)
list_best_corr = rev(sort( cor(Boston)[,'crim_0_or_1'] ))
print(list_best_corr)




