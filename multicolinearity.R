#MULTICOLINEARITY

#1. Effects of High Colinearity

x1 = rnorm(100)
x2 = rnorm(100, mean=x1, sd=0.01)
print(sprintf("X1 and x2 are %10.6f correlated ", cor(x1,x2)))
y = rnorm(100,mean=3+x1+x2, sd=0.2)
DF = data.frame(y=y, x1=x1, x2=x2)
model = lm(y ~ x1+x2, data=DF)
summary(model)

# When running linear regression for the first time is easy to see that the coefficents estimation is not very good, their standard error is high and they are not statiscitacally significant. When running all the code several times, the same bad characteristics remain and everytime different coefficents are calculated even if the relationship is quided by the same equation. In fact, the coefficents are so poorly calculated that sometimes their relationship with the output variable goes in the opposite direction. This exercise shows the problems described in the literature about colinearity.


#2. Quick solutions to solve Colinearity

#(a)Using the model previous model, 100 times more samples(observations)are needed to have realiable coefficients.

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

print(sprintf("X1 and x2 are %10.6f correlated ", cor(x1,x2)))
print(sprintf("X9 and x10 are %10.6f correlated ", cor(x1,x2)))
y = rnorm(100,mean=3+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10, sd=0.2)
DF = data.frame(y=y, x1=x1, x2=x2,x3=x3,x4=x4,x5=x5,x6=x6,x7=x7,x8=x8,x9=x9,x10=x10)
model = lm(y ~ x2+x3+x4+x5+x6+x7+x8+x10, data=DF)
summary(model)

#When leaving out one of the variables in each colinear pair, the coefficients of the linear regression are stable. The coeficients of the variables of the colinear pairs that remained in de model endedup carrying the weight of the missing variable.

#(c) The problem eith the findings on (a) is that 


install.packages("ISLR")
install.packages("boot")
library(boot)
library(ISLR)

#(a)&(b)
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)
plot(x,y)

#(c)
DF = data.frame(y=y, x=x)


#The 4 models provided:
m_i = glm(y ~ x, data=DF)
m_ii = glm(y ~ x + I(x^2), data=DF)
m_iii = glm(y ~ x + I(x^2) + I(x^3), data=DF)
m_iv = glm(y ~ x + I(x^2) + I(x^3) + I(x^4), data=DF)

# Do cross-validation on each model:
cv.err_iv = cv.glm( DF, m_iv )
print( sprintf( "Model (iv): cv output= %10.6f", cv.err_iv$delta[1] ) )



set.seed(222)
x = c(rnorm(100))
y = c(x-2*x^2+rnorm(100))
df = data.frame(x,y)
cv.err = rep(0,4)
for (i in 1:4){
  lr.fit = glm(y~poly(x,degree=i,raw=TRUE))
  cv.err[i] = cv.glm(df, lr.fit)$delta[1]
}
cv.err

