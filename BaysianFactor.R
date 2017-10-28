data("stackloss")
stackloss

OLS_LR = lm(stack.loss~., data=stackloss) # OLS linear regression
summary(OLS_LR)

baysian_LR = regressionBF(stack.loss~., data=stackloss) # Baysian Learning

baysian_LR
head(baysian_LR)
plot(baysian_LR)
max(baysian_LR) # Best fitting model with the highest BF
baysian_LR["Air.Flow"] # bausian model with only Air.Flow as regressor
# dropping a predictor from the full model
baysian_LR2 <- regressionBF(stack.loss~., data=stackloss, whichModels = "top")
baysian_LR2

# posterior estimation

baysian_Best = lmBF(stack.loss~Air.Flow + Water.Temp, data=stackloss)
baysian_Best
summary(baysian_Best)

bestModelPosterior = posterior(baysian_Best, iterations = 10000)
summary(bestModelPosterior)

#===========end of demo =================
#=========== start of the real analysis of Soccer data ===
str(df.edf.1)

modelo.1 = 'AggregatedAttendance~MHG + MAG + LMV + LMD + LWS + LLS'
modelo.2 = 'AggregatedAttendance~MHG + MAG + LMV + LMD + LUBS + LWLSS'

OLS_SOC = lm(modelo.2, data=df.edf)
summary(OLS_SOC)

Baysian_SOC = regressionBF(AggregatedAttendance~MHG + MAG + LMV + LMD + LWS + LLS, data=df.edf) # calculate BF of regression models
Baysian_SOC = regressionBF(AggregatedAttendance ~ LMV + LUBS + LWLSS, data=df.edf)
#Baysian_SOC
head(Baysian_SOC) #/ max(Baysian_SOC)

Baysian_SOC2 = regressionBF(AggregatedAttendance~., data=df.edf, whichModels = "top")
Baysian_SOC2

Baysian_SOC_Post = lmBF(AggregatedAttendance~MHG + LMV + LUBS + LWLSS, data=df.edf)
Baysian_SOC_Post

summary(posterior(Baysian_SOC_Post, iterations=10000))

#================= create a small sample of the whole 874 records =======================
nrow(df.edf)
set.seed(12346)
mini_soc = sample(1:nrow(df.edf), 200, replace = FALSE)
mini_soc

df.edf.1 = df.edf[-soc.sub,]
#=================================pick sample data by year ==============================
soc.sub = which(ddf$Season=="2007") #| ddf$Season == "2002" | ddf$Season == "2003")

#========== read modelstring and assess BIC ==========================
