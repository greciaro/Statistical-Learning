y = 2
lambda = 2
betas = seq(-10, 10, 0.1)
func = (y - betas)^2 + lambda * betas^2
plot(betas, func, pch = 20, xlab = "beta", ylab = "Ridge optimization")
# X-axis
axis(1, at = c(-2 ,-1, 1, 2))
est.beta = y/(1 + lambda)
est.func = (y - est.beta)^2 + lambda * est.beta^2
points(est.beta, est.func, col = "red", pch = 4, lwd = 5, cex = est.beta)


