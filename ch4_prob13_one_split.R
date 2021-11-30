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
print( "Columns ordered by their correlation with crim above or below median" )
print("--------------------------------------------------------")
print("Best three columns:")
print(list_best_corr)

three_best_corr = names(list_best_corr)[2:4]
print(three_best_corr)


#######################LOOP BEGINS##############################

#Divide data set, between the trainning and testing data sets
splitting_portion = 3*n/4
inds_train = sample(1:n,splitting_portion)
inds_test = (1:n)[-inds_train]
Boston_train = Boston[inds_train,]
Boston_test = Boston[inds_test,]

#####Fit models and calculate test errors

#Logistic Regression
lr_model = glm( crim_0_or_1 ~ nox + rad + dis, data=Boston_train, family=binomial)
lr_prediction = predict( lr_model, newdata=Boston_test, type="response" )
lr_binary_prediction = rep( 0, length(lr_prediction))
lr_binary_prediction[ lr_prediction > 0.5 ] = 1
lr_p_vs_t = table(predicted=lr_binary_prediction, truth=Boston_test$crim_0_or_1)
print( "Logistic Regression" )
print( "Error or Confusion Matrix" )
print(lr_p_vs_t)
lr_perc_correct = 100 * ( lr_p_vs_t[1,1] + lr_p_vs_t[2,2] ) / sum(lr_p_vs_t) 
print(lr_perc_correct)
print( sprintf( "Percentage correct= %10.6f", lr_perc_correct ))
print("_________________________________________________________")

#Linear Discriminant Analysis (LDA)
lda_model = lda(crim_0_or_1 ~ nox + rad + dis, data=Boston_train)
lda_prediction = predict(lda_model, newdata=Boston_test)
lda_p_vs_t = table(predicted=lda_prediction$class, truth=Boston_test$crim_0_or_1)
print( "Linear Discriminant Analysis (LDA)" )
print( "Error or Confusion Matrix" )
print(lda_p_vs_t)
lda_perc_correct = 100 * (lda_p_vs_t[1,1] + lda_p_vs_t[2,2]) / sum(lda_p_vs_t) 
print(lda_perc_correct)
print( sprintf( "Percentage correct= %10.6f", lda_perc_correct ))
print("_________________________________________________________")

#Quadratic Discriminant Analysis (QDA)
qda_model = qda(crim_0_or_1 ~ nox + rad + dis, data=Boston_train)
qda_prediction = predict( qda_model, newdata=Boston_test) 
qda_p_vs_t = table(predicted=qda_prediction$class, truth=Boston_test$crim_0_or_1)
print( "Quadratic Discriminant Analysis (QDA)" )
print( "Error or Confusion Matrix" )
print(qda_p_vs_t)
qda_perc_correct = 100 * (qda_p_vs_t[1,1] + qda_p_vs_t[2,2]) / sum(qda_p_vs_t) 
print( sprintf( "Percentage correct= %10.6f", qda_perc_correct ))
print("_________________________________________________________")

#k-Nearest Neighbour Classification (kNN) k=1
X_train = Boston_train; X_train$crim_0_or_1 = NULL
Y_train = Boston_train$crim_0_or_1
X_test = Boston_test; X_test$crim_0_or_1 = NULL
k1_prediction = knn( X_train, X_test, Y_train, k=1 )
k1_p_vs_t = table(predicted=k1_prediction, truth=Boston_test$crim_0_or_1) 
print( "k-Nearest Neighbour Classification (kNN) k=1" )
print( "Error or Confusion Matrix" )
print(k1_p_vs_t)
k1_perc_correct = 100 * (k1_p_vs_t[1,1] + k1_p_vs_t[2,2]) / sum(k1_p_vs_t)
print( sprintf( "Percentage correct= %10.6f", k1_perc_correct ))
print("_________________________________________________________")


#k-Nearest Neighbour Classification (kNN) k=3
k3_prediction = knn( X_train, X_test, Y_train, k=3 )
k3_p_vs_t = table(predicted=k3_prediction, truth=Boston_test$crim_0_or_1) 
print( "k-Nearest Neighbour Classification (kNN) k=3" )
print( "Error or Confusion Matrix" )
print(k3_p_vs_t)
k3_perc_correct = 100 * (k3_p_vs_t[1,1] + k3_p_vs_t[2,2]) / sum(k3_p_vs_t) 
print( sprintf( "Percentage correct= %10.6f", k3_perc_correct ))
print("_________________________________________________________")
