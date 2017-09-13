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

cvffit = cv.glmnet(x, y, type.measure = "mse", nfolds = 20)

coef(cvffit, s = "lambda.min")
predict(cvffit, newx=x[1:5,], s="lambda.min")

foldid = sample(1:10, size=length(y), replace=TRUE)

# choose the best alpha

cv1 = cv.glmnet(x,y, foldid = foldid, alpha=1)
cv.5 = cv.glmnet(x,y, foldid = foldid, alpha=0.5)
cv0 = cv.glmnet(x,y, foldid = foldid, alpha=0)

par(mfrow=c(2,2))
plot(cv1);plot(cv.5);plot(cv0)


plot(log(cv1$lambda), cv1$cvm, pch=19, col="red", xlab="log(lambda)", ylab=cv1$name)

points(log(cv.5$lambda), cv.5$cvm, pch=22, col="green")
points(log(cv0$lambda), cv0$cvm, pch=22, col="blue")

legend("topleft", legend=c("alpha = 1", "alpha = 0.5","alpha = 0"), pch=16, col=c("red", "green","blue"))

str(y.s)

# convert sas data into numerical vector

y.s = as.numeric(european_soccer_leagues$AggregatedAttendance)

x.s = as.matrix(european_soccer_leagues[,4:11])

# normalize variables

x.s.std = scale(x.s, center = colMeans(x.s), scale=colMeans(x.s))
y.s.std = scale(y.s)

str(x.s)
#==========Baseline OLS Regression Model ==========
edf = european_soccer_leagues[,c(4:11,15)]
m.edf = as.matrix(edf)
s.edf = scale(m.edf, center=colMeans(m.edf), scale=colMeans(m.edf)) # normalized matrix

df.edf = as.data.frame(s.edf)
model = AggregatedAttendance ~ .
fit = lm(model, data=df.edf)
print(fit)
coef(fit)
anova(fit)
influence(fit)
confint(fit, level=0.95)
summary(fit)

# relative importance calcuation ====
#linmod = lm(Fertility ~., data=swiss)

metrics <- calc.relimp(linmod, type=c("pmvd"))

metrics
plot(metrics)
#==CV Regression Mopdel ====
cvols = cv.lm(data=df.edf, fit, m=5)
summary(cvols)
print(cvols)
plot(cvols)
attr(cvols, "ms")
#========== LASSO =====
y = s.edf[,9]
x = s.edf[,1:8]

y[1:4]
x[1:4,1:8]

fit.s = glmnet(x, y)

plot(fit.s)
print(fit.s)
coef(fit.s, s=0.1)

cvfit.s = cv.glmnet(x, y, alpha=0.0)
plot(cvfit.s)
cvfit.s$lambda.min
coef(cvfit.s, s="lambda.min")
summary(cvfit.s)
print(cvfit.s)
#==============================
