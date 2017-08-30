data(QuickStartExample)
x[1:10,]
NCOL(x)
NROW(x)
str(y)
str(x)
fit <- glmnet(x,y)
plot(fit)
print(fit)

coef(fit, s=0.1)

nx <- matrix(rnorm(10*20), 10, 20)
predict(fit, newx = nx, s=c(0.1,0.05))
