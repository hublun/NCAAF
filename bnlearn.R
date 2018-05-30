#ET5$AATT = ET5$AggregatedAttendance / ET5$LargestAttendance
#ET5$AATT

#discretize()
#============================= ET5 Data Input ==============================
#ET5.1 <- log(na.omit(ET5[,c(6:13, 18)]))  # remove records with NA and missing values

#colnames(ET5.1)[1] <- "MHG"
#colnames(ET5.1)[2] <- "MAG"
#colnames(ET5.1)[3] <- "LMV"
#colnames(ET5.1)[4] <- "LMD"
#colnames(ET5.1)[5] <- "LWS"
#colnames(ET5.1)[6] <- "LUS"
#colnames(ET5.1)[7] <- "LLS"
#colnames(ET5.1)[8] <- "LLDS"
#colnames(ET5.1)[9] <- "AATT"
#colnames(ET5.1)
#head(ET5.1)

#ET5.2 <- scale(ET5.1, center = colMeans(ET5.1), scale=colSD(ET5.1))
#ET5.2 <- scale(ET5.1, center = colMeans(ET5.1)) # center only

#ET5.3 <- data.frame(ET5.2) # change matrix to dataframe

#head(ET5.3)

#ET5.4 <- ET5.1[,-c(6,8)]  # create not draw set 
#head(ET5.4)
#======== Check Distribution =====================

ggplot(data=ET5.4, mapping=aes(x = MAG)) +
  geom_histogram(aes(y=..density..), col="blue", fill="green", bins = 20) +
  geom_density()

#---------------- Training and Testing Data Split -------------

#---------------------------------------
set.seed(8899)

inTrain <- createDataPartition(
  y=ET5.4$AATT,
  p=0.8,
  list = FALSE
)

data.fit = ET5.4[inTrain,]  # training set

data.eval  = ET5.4[-inTrain,] # testing dataset
#============ Paralell Computing ==========================
registerDoMC(cores=4)


#================ Caret Package Mrthods ===================
crtl <- trainControl(method = "repeatedcv", repeats = 3)
#----------------------------------------------------------
fit.Caret <- train(
  AATT~.,
  data = data.fit,
  method = "glm",
  preProc = c("center","scale"),
  tuneLength = 15,
  trControl = crtl  # repeated cross-validation
  #params = list(split="information")
)

#-----------------------------------------------

fit.Caret <- train(
  AATT~.,
  data = data.fit,
  method = "rpart2",
  preProc = c("center","scale"),
  tuneLength = 15,
  trControl = crtl  # repeated cross-validation
  #params = list(split="information")
)

#----------------------------------------

fit.Caret <- train(
  AATT~.,
  data = data.fit,
  method = "nnet",
  lineout = TRUE,
  trace = TRUE
)

#-------------- evaluate performance ---------------------
pred.Caret <- predict(fit.Caret, newdata = data.eval)

ggplot(fit.Caret)

RMSE(pred.Caret, data.eval$AATT)

remove(fit.Caret, pred.Caret)

#================= Score based bnlearn Algorithm===============

ETL.1 <- hc(ET5.4, score="bic-g") # Hill-Climbing learning
fit.bn <- hc(data.fit) # hybrid-learning algorithm
modelstring(fit.bn)
graphviz.plot(fit.bn)

score(fit.bn, data = ET5.4)

#=========== predicting attendance based on learned network ====

pred.2 <- predict(fit.bn, data = data.eval, node = "AATT", method = "bayes-lw")

RMSE(pred.2, data.eval$AATT)
#============= Compute RMSE root mean square error ========================

rmse = function(y, yp){
 delta = y - yp;
 return(sqrt(sum(delta^2)/length(delta)))
}

rmse(data.test$AATT, pred.1)
rmse(data.test$AATT, pred.2)

#-------------- construct network ---------------------------------------------------

dag = model2network("[MHG][MAG][LMD][LMV][LWS][LLS][AATT|MHG:LMV:LMD:MAG:LLS:LWS]")
dag.1 = model2network("[MHG][MAG][LMD][LMV][LWS|MHG][LLS|MHG][AATT|MHG:MAG:LLS:LWS]")
dag.2 = model2network("[AATT][MHG|AATT][MAG|AATT][LMD|AATT][LMV|AATT][LWS|AATT][LLS|AATT]")

mdag <- moral(dag.1) #moral graph
graphviz.plot(mdag)
fit = bn.fit(dag.1, data.train)

str(fit.1)

print(fit.1)

score(bnet.1, data.test, type="bic-g")
#------------- Cross-Validation ---------------------------------------

bn.cv(bn = "hc", data=ET5.4, loss.args = list(target="AATT"), loss = "mse", runs=10, method = "k-fold") # 10-fold cross validation
bn.cv(dag.1, data=ET5.4, loss.args = list(target="AATT"), loss = "mse", runs=10, method = "k-fold") # 10-fold cross validation

cv.lm(data=ET5.4, form.lm = formula(AATT ~ .), m=10)

sqrt(0.002083162)

#============================= Correlation Matrix ==========================
write.csv(df.edf, file = "SAS_File.csv")
#============================= Regression Tree =============================

#========= building a network from model string ===================

  
#============ fit parameters of a baysian network =========================
 
 
# condictional independence test
ci.test("MAG", "MHG", data=data.train, test = "smc-cor")

#========== How to read a file of choice ================
file.choose() # which opens up the file choose dailogure
#========================================================

