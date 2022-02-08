#MULTICOLINEARITY 

set.seed(1)
x = rnorm(100)
y = 2 * x + rnorm(100)
plot(x,y)

x1 = 0.5 * x
x2 = 0.7 * x
x3 = 0.8 * x  
x4 = 0.85 * x
x5 = 0.9 * x
x6 = 0.95 * x
x7 = 0.99 * x

data_frame = data.frame(y=y, x=x, x1=x1, x2=x2, x3=x3, x4=x4, x5=x5, x6=x6, x7=x7)

original_model = glm(y ~ x, data=data_frame)
summary(original_model)
o_beta1 = original_model$coefficients[2]
o_SE = 
original_model$offset


model_1 = glm(y ~ x + 1, data=data_frame)
model_1$coefficients