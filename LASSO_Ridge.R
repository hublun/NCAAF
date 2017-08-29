data(QuickStartExample)
x[1:10,]
NCOL(x)
NROW(x)
str(y)
str(x)
fit <- glmnet(x,y)
plot(fit)
