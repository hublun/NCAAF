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

#cross-validation model

cvfit <- cv.glmnet(x, y)
plot(cvfit)

cvfit$lambda.min
cvfit$lambda.1se
coef(cvfit, s="lambda.min")

predict(cvfit, newx = x[1:5,], s= "lambda.min")

ffit = glmnet(x=x, y=y, weights=c(rep(1,50), rep(2,50)), alpha = 0.2, nlambda=20)

print(ffit, digits=3)
plot(ffit, xvar = "lambda", label=TRUE)
plot(ffit, xvar="dev", label = TRUE)

any(fit$lambda==0.5)


coef.exact = coef(ffit, s=0.5, exact=TRUE)
coef.apprx = coef(ffit, s=0.5, exact=FALSE)

cbind2(coef.exact, coef.apprx)

predict(ffit, newx=x[1:5,], type= "response", s=0.04, exact=FALSE)
