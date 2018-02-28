#=============== factorizing the phishing data set ============
Phishing_Data_Encoded$Lf <- factor(Phishing_Data_Encoded$O) 
Phishing_Data_Encoded$Ffd7 <- factor(Phishing_Data_Encoded$D7) 
colnames(Phishing_Data_Encoded)[50]

Fish <- Phishing_Data_Encoded[,32:62]


#============ CHAID Package ==================================
#install.packages("CHAID", repos = "http://R-Forge.R-project.org", type = "source") #install CHAID from source

model.1 = {Lf ~ Ffab1 + Ffab2 + Ffab3 +Ffab4 + Ffab5 + Ffab6 + Ffab7+ Ffab8+ Ffab9+ Ffab10+ Ffab11+ Ffab12}
model.2 = {Lf ~ Ffa1 + Ffa2 + Ffa3 +Ffa4 + Ffa5 + Ffa6}
model.3 = {Lf ~ Ffhj1 + Ffhj2 + Ffhj3 +Ffhj4 + Ffhj5}
model.4 = {Lf ~ Ffd1 + Ffd2 + Ffd3 +Ffd4 + Ffd5 + Ffd6 + Ffd7}
model = {Lf ~ .}

print(Chd)
plot(Chd)
length(Chd)
depth(Chd)
#====================== Predict and Confusion Matrix ================
set.seed(1001)

index.t <- sample(nrow(Fish), round(nrow(Fish)*0.8), replace = FALSE)

length(index.t)



index.tr

dataset.test <- Fish[-index.t,]

for (i in 1:1000){
  index.tr <- index.t[sample(length(index.t), round(length(index.t)*0.8), replace = FALSE)]
  Chd <- chaid(model, data = Fish[index.tr,])
  o <- ML.metrcs()
  cat(i, "\t", o[1], "\t", o[2], "\n", file = "PBS1000.csv", append = TRUE)
  }
#====================================================================





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

