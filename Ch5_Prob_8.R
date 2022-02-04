#CHATER 5
#PROBLEM 8

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

