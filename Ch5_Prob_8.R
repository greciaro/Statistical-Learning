#CHATER 5
#PROBLEM 8

set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)
plot(x,y)
