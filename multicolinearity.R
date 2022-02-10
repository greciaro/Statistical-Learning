#MULTICOLINEARITY

#1. Effects of High Colinearity

x1 = rnorm(100)
x2 = rnorm(100, mean=x1, sd=0.01)
print(sprintf("X1 and x2 are %10.6f correlated ", cor(x1,x2)))
y = rnorm(100,mean=3+x1+x2, sd=0.2)
DF1 = data.frame(y=y, x1=x1, x2=x2)
model = lm(y ~ x1+x2, data=DF1)
summary(model)

# When running linear regression for the first time is easy to see that the coefficents estimation is not very good, their standard error is high and they are not statiscitacally significant. When running all the code several times, the same bad characteristics remain and everytime different coefficents are calculated even if the relationship is quided by the same equation. In fact, the coefficents are so poorly calculated that sometimes their relationship with the output variable goes in the opposite direction. This exercise shows the problems described in the literature about colinearity.


#2. Quick solutions to solve Colinearity

#(a)Using the previous model, 1000 times more samples(observations) are needed to have realiable coefficients.

#(b)Creating a new data set with 10 input variables.
x1 = rnorm(100)
x3 = rnorm(100)
x4 = rnorm(100)
x5 = rnorm(100)
x6 = rnorm(100)
x7 = rnorm(100)
x8 = rnorm(100)
x9 = rnorm(100)
x2 = rnorm(100, mean=x1, sd=0.01)
x10 = rnorm(100, mean=x9, sd=0.01)
y = rnorm(100,mean=3+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10, sd=0.2)
DF2 = data.frame(y=y, x1=x1, x2=x2,x3=x3,x4=x4,x5=x5,x6=x6,x7=x7,x8=x8,x9=x9,x10=x10)

cor(DF2)

print(sprintf("X1 and x2 are %10.6f correlated ", cor(x1,x2)))
print(sprintf("X9 and x10 are %10.6f correlated ", cor(x1,x2)))


model = lm(y ~ x2+x3+x4+x5+x6+x7+x8+x10, data=DF2)
summary(model)

#When leaving out one of the variables in each colinear pair, the coefficients of the linear regression are stable. The coeficients of the variables of the colinear pairs that remained in the model endedup carrying the weight of the missing variable.

#(c) The problem with the findings on (a) is that is very unlikely that enough resources (time, money, etc.) are available to increase the sample size one thousand fold. The problem with getting good results from the linear regression when ignoring variables is that your model will have a specification bias when future predictions are needed.


#3. When Colinearity is a problem?

counter = seq(from = 0.01, to = 100, by = 0.1)
n = length(counter)
x1_matrix = matrix(0,100,n)
x2_matrix = matrix(0,100,n)
cor_vector = matrix(0,1,n)
y_matrix = matrix(0,100,n)
beta1_vector = matrix(0,1,n)

for (i in 1:n){
  x1_vector[,i] = rnorm(100)
  x2_vector[,i] = rnorm(100, mean=x1_vector[,i], sd=counter[i])
  cor_vector[i] = cor(x1_vector[,i],x2_vector[,i])
  y_matrix[,i] = rnorm(100,mean=3+x1_vector[,i]+x2_vector[,i], sd=0.2)
  beta1_vector[i] = lm(y_matrix[,i] ~ x1_vector[,i]+x2_vector[,i])$coef[2]
}

plot(cor_vector,beta1_vector, lwd=2, pch=19, xlim=c(0.5, 1), ylim=c(-3, 3))

# With a very well defined data set like the one in this section, the plot show that beta1 becomes unstable at high levels of correlation (above 0.9) between x1 and x2. Based on the literature, the recommended treshold for colinearity is as low as 0.85, depending on the statistical significance of the results.

#4. Linear Regression on a Data Set with Multicolinearity
x1 = rnorm(100)
x2 = rnorm(100)
x3 = rnorm(100)
x4 = rnorm(100)
x5 = x1+x2+x3+x4
x6 = rnorm(100)
x7 = rnorm(100)
x8 = rnorm(100)
x9 = rnorm(100)
x10 = x6+x7+x8+x9
x11 = rnorm(100)
x12 = rnorm(100)
x13 = rnorm(100)
x14 = rnorm(100)
x15 = x1+x12+x13+x14
x16 = rnorm(100)
x17 = rnorm(100)
x18 = rnorm(100)
x19 = rnorm(100)
x20 = x16+x17+x18+x19

y = rnorm(100,mean=x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20, sd=0.2)

DF4 = data.frame(y=y, x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6,x7=x7,x8=x8,x9=x9,x10=x10,x11=x11,x12=x12,x13=x13,x14=x14,x15=x15,x16=x16,x17=x17,x18=x18,x19=x19,x20=x20)

model = lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20, data=DF4)

summary(model)

# The output variable "y" cannot be calculated by linear regression as the beta values (coefficients) of the input variables that have multicolinearity are indetermined. The difference between these results and the one in Section two are that linear regression can model a data set with colineary but not with multicolinearity.

#5. Multicolinearity Diagnose

#The corrleation matrix is limited to find pairwise correlations, theefore it will only find colinearity.

#Eigensystem Analysis
eig_values =eigen(cor(DF4))$values
#Eigen values vary in magnitude from large to small, therefore, there is multicolinearity.

#Condition Number
cond_num = abs(max(eig_values)/min(eig_values))
#Condition Number is bigger than 100

#Variance Inflation Factor
install.packages("car")
library(car)
vif_vector = vif(model)
#VIF cannot be calculated becuse requires to perform linear regression, which we previosly saw its not possible with this dataset.This means, there is not only multicolinearity but also its perfect!!! OMG! There a perfect linear relationship between groups of variables.


#6. Multicolinearity Remediation(Part 1)

#(a) Principal Component Analysis (PCA)
DF4_only_x = DF4[,-1]
DF4_only_x_pca = prcomp(DF4_only_x, center = TRUE, scale = TRUE)
plot(DF4_only_x_pca)
screeplot(DF4_only_x_pca, type="line",main="Scree Plot")
summary(DF4_only_x_pca)
DF4_only_x_pca$rotation #how PCs are made from the input variables
eigen(cor(DF4_only_x))$values#eigen values of original data
diag(var(DF4_only_x_pca$x[,]))#variance of each PC
DF4_pca = cbind(DF4[,1],data.frame(DF4_only_x_pca$x))
colnames(DF4_pca)[1] = "y_pca"
cor(DF4_pca)[,1]
model = lm(y_pca ~., data = DF4_pca)
summary(model)#PCs 9,17,8,19 and 20 are not statiscally significant, which means they don't contribute to the prediction of y
betas = DF4_only_x_pca$rotation %*% model$coefficients[-1]
betas # Indluding PC's that are not relevant affects the quantification of betas 

model_2 = lm(y_pca ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC10+PC11+PC12+PC13+PC14+PC15+PC16, data = DF4_pca)
summary(model_2)
rotation_2 = DF4_only_x_pca$rotation[1:16,1:16]
rotation_2 = rotation_2[-9,-9]
betas_2 = rotation_2 %*% model_2$coef[-1]
betas_2


#(b) Factor Analysis (FA)
DF4_only_x_fa = factanal(DF4_only_x[,1:16], factor = 10)



#7. Multicolinearity Remediation(Part 2)

#(a)Ridge Regression
install.packages("MASS")
library(MASS)
lambdas = seq(0,1,0.001)
models = lm.ridge(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20, data=DF4,lambda = lambdas)
select(models)
model = lm.ridge(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20, data=DF4,lambda = 0.011)
model

# The best penalty was 0.011 and we found the beta values for 3 variables are almost zero which means their contribution will be minimal or none.


#(b)Lasso Regression
install.packages("glmnet")
install.packages("tidyverse")
library(glmnet)
library(tidyverse)
library (broom)
lambdas = 10^seq(2, -3, by = -.1)
models = cv.glmnet(DF4[-1],y,alpha = 1,lambda = lambdas, standardize = TRUE, nfolds = 10)
lambda_best = min(models$lambda)
lambda_best

models$lambda



cv_models = cv.glmnet(DF4[-1],y,alpha = 1,lambda = lambdas)

x_matrix = DF4[-1]
y
