install.packages("ISLR")


# Part (a):
#
set.seed(1)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)
plot( x, y )

DF = data.frame( y=y, x=x )



# Do cross-validation on each model:
library(ISLR)
Data = data.frame(x, y)
set.seed(1)
# i.
glm.fit = glm(y ~ x)
cv.glm(Data, glm.fit)$delta




model_1 = glm( y ~ x, data=DF )
cv.err = cv.glm( DF, model_1 )
print( sprintf( "Model (i): cv output= %10.6f", cv.err$delta[1] ) )

model_2 = glm( y ~ x + I(x^2), data=DF )
cv.err = cv.glm( DF, model_2 )
print( sprintf( "Model (ii): cv output= %10.6f", cv.err$delta[1] ) )

model_3 = glm( y ~ x + I(x^2) + I(x^3), data=DF )
cv.err = cv.glm( DF, model_3 )
print( sprintf( "Model (iii): cv output= %10.6f", cv.err$delta[1] ) )

model_4 = glm( y ~ x + I(x^2) + I(x^3) + I(x^4), data=DF )
cv.err = cv.glm( DF, model_4 )
print( sprintf( "Model (iv): cv output= %10.6f", cv.err$delta[1] ) )
