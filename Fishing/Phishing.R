#=============== factorizing the phishing data set ============
Phishing_Data_Encoded$Lf <- factor(Phishing_Data_Encoded$O) 
Phishing_Data_Encoded$Ffa6 <- factor(Phishing_Data_Encoded$A6) 
colnames(Phishing_Data_Encoded)[50]

Fish <- Phishing_Data_Encoded[,32:50]


#============ CHAID Package ==================================
#install.packages("CHAID", repos = "http://R-Forge.R-project.org", type = "source") #install CHAID from source

model.1 = {Lf ~ Ffab1 + Ffab2 + Ffab3 +Ffab4 + Ffab5 + Ffab6 + Ffab7+ Ffab8+ Ffab9+ Ffab10+ Ffab11+ Ffab12}
model.2 = {Lf ~ Ffa1 + Ffa2 + Ffa3 +Ffa4 + Ffa5 + Ffa6}

model = {Lf ~ .}

Chd <- chaid(model, data = Fish[index.t,])

print(Chd)

plot(Chd)

length(Chd)

depth(Chd)
#====================== Predict and Confusion Matrix ==========
nrow(Fish)*0.8

round(nrow(Fish)*0.8)

index.t <- sample(nrow(Fish), round(nrow(Fish)*0.8), replace = FALSE)
index.t



dataset.test <- Fish[-index.t,]

ML.metrcs()
#================= Function construct confusion matrix ========================
ML.metrcs <- function(model = Chd, newdata = dataset.test){
  pred <- predict(Chd, newdata = dataset.test)
  label.real <-dataset.test$Lf 
  class(pred)
  pred
  class(label.real)

  tab <- table(label.real, pred)

  tp = tab[1]
  tn = tab[4]
  fp = tab[2]
  fn = tab[3]

  acc = (tp+tn)/(tp+tn+fp+fn)
  acc
  alpheta = fp /(fn+0.001)
  alpheta 
  return (c(acc, alpheta))
}
#====================== end of ML assessment =========================
