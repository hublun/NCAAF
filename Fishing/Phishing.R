#=============== factorizing the phishing data set ============
Phishing_Data_Encoded$Lf <- factor(Phishing_Data_Encoded$O) 
Phishing_Data_Encoded$Ffab12 <- factor(Phishing_Data_Encoded$AB12) 
colnames(Phishing_Data_Encoded)[44]
Fish <- Phishing_Data_Encoded[,32:44]


#============ CHAID Package ==================================
#install.packages("CHAID", repos = "http://R-Forge.R-project.org", type = "source") #install CHAID from source

Chd <- chaid(Lf~Ffab1 + Ffab2, data = Fish)
print(Chd)
plot(Chd)

length(Chd)

depth(Chd)

Chd[3]
