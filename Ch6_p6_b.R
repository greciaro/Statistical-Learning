y = 2
lambda = 2
betas = seq(-3, 3, 0.01)
func = (y - betas)^2 + lambda * abs(betas)
plot(betas, func, pch = 20, xlab = "beta", ylab = "Lasso optimization")

est.beta = y - lambda/2
est.func = (y - est.beta)^2 + lambda * abs(est.beta)
points(est.beta, est.func, col = "red", pch = 4, lwd = 5, cex = est.beta)