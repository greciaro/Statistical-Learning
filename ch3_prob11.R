
#_____________Creating the true model for the problem______________________

#Create a generator or random numbers that always starts from the number one

set.seed (1)

# the independet variable or predictor is "x"
# x = 100 random numbers with normal distribution

x = rnorm(100)

# the dependent variable or response is "y"
# y = 2x + other 100 random numbers with normal distribution

y = 2*x + rnorm(100)

#_________________________    11.a)   _____________________________________

# Perform a simple linear regression of y onto x, without an intercept:

lm.y_onto_x = lm(y~x+0)

summary(lm.y_onto_x)

#beta coeficient = multiplier = 1.99
#Std. Error =0.1065
#t-statistic = t value = 18.73
#p-value associated with the null hypothesis = Pr(>|t|) = almost zero

#_________________________    11.b)   _____________________________________

# Perform a simple linear regression of x onto y, without an intercept:

lm.x_onto_y = lm(x~y+0)

summary(lm.x_onto_y)

#beta coeficient = multiplier =
#Std. Error =
#t-statistic = t value = 
#p-value associated with the null hypothesis = Pr(>|t|) =

#_________________________    11.c)   _____________________________________
#
#What is the relationship between the results obtained in (a) and (b)?


#_________________________    11.d)   _____________________________________
#
#Calculate algebraically t-statistic for the regression of Y onto X without an intercept:

#ppt

#Calculate numerically t-statistic for the regression of Y onto X without an intercept:

t_value = (sqrt(length(x)-1) * sum(x*y)) / (sqrt(sum(x*x) * sum(y*y) - (sum(x*y))^2))

print(t_value)

#_________________________    11.e)   _____________________________________
#
# Argue that the t-statistic for the regression of y onto x is the same as the t-statistic for the regression of x onto y.

#_________________________    11.f)   _____________________________________
#
#Show that when regression is performed with an intercept, the t-statistic for the null hypothesis of beta1 is the same for the regression of y onto x as it is for the regression of x onto y.

lmlm.y_onto_x_with_intercept = lm(y~x)
lmlm.x_onto_y_with_intercept = lm(x~y)
summary(lmlm.y_onto_x_with_intercept)
summary(lmlm.x_onto_y_with_intercept)
