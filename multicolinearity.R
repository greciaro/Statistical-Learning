#MULTICOLINEARITY 

#The true data relationship
set.seed(1)
x = rnorm(100)
y = x + x^2 + x^3 + x^4 + rnorm(100)*x^5 + rnorm(100)
plot(x,y)

#Variables with different colinearity level (ex. 0.5 =medium , 0.99 = strong) with the variable x
x1 = 0.5 * x
x2 = 0.7 * x
x3 = 0.8 * x  
x4 = 0.85 * x
x5 = 0.9 * x
x6 = 0.95 * x
x7 = 0.99 * x

#Organizing all data in a "table"
data_frame = data.frame(y=y, x=x, x1=x1, x2=x2, x3=x3, x4=x4, x5=x5, x6=x6, x7=x7)

#Visualizing  Original function beta1 and its SE and Pvalue
original_model = glm(y ~ x, data=data_frame)
summary(original_model)

#creating vectors to store beta1 and its SE and Pvalue per model

model_7 = glm(y ~ x + x7, data=data_frame)
summary(model_7)

o_beta1 = summary(original_model)$coefficients[2,1]
o_SE = summary(original_model)$coefficients[2,2]
o_Pvalue = summary(original_model)$coefficients[2,4]


# model_1 = glm(y ~ x + 1, data=data_frame)
# model_1$coefficients