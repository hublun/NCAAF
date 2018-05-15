
#============================= ET5 Data Input ==============================
ET5.1 <- log(na.omit(ET5[,c(6:13, 17)]))  # remove records with NA and missing values




colnames(ET5.1)[1] <- "MHG"
colnames(ET5.1)[2] <- "MAG"
colnames(ET5.1)[3] <- "LMV"
colnames(ET5.1)[4] <- "LMD"
colnames(ET5.1)[5] <- "LWS"
colnames(ET5.1)[6] <- "LUS"
colnames(ET5.1)[7] <- "LLS"
colnames(ET5.1)[8] <- "LLDS"
colnames(ET5.1)[9] <- "AATT"
colnames(ET5.1)
head(ET5.1)

ET5.2 <- scale(ET5.1, center=colMeans(ET5.1), scale=colMeans(ET5.1))
ET5.3 <- data.frame(ET5.2) # change matrix to dataframe
head(ET5.3)
ET5.4 <- ET5.3[,-c(6,8)]  # create not draw set 
head(ET5.4)
#======== Check Distribution =====================
ggplot(data=ET5.4, mapping=aes(x = LLS)) +
  geom_histogram(aes(y=..density..), col="blue", fill="green") +
  geom_density()
#================= test run bnlearn ===============
ETL.1 <- hc(ET5.4, score="bic-g") # Hill-Climbing learning
ETL.2 <- iamb(ET5.4)

plot(ETL.1)
score(ETL.2, data = ET5.4)
#-------------- construct network -----------------
dim(ET5.4)[1]

#============================= Correlation Matrix ==========================
write.csv(df.edf, file = "SAS_File.csv")
#============================= Regression Tree =============================
tree.att <- rpart(AggregatedAttendance~., data=df.edf, method="anova", xval=5)
printcp(tree.att) 
text(tree.att)
plot(tree.att)
rsq.rpart(tree.att)
summary(tree.att, compress=TRUE)

# produce a picture with labels in it

pdf("rparttree.pdf", width=1200, height=800)
post(tree.att, file="", title="werty", bp=18)
dev.off()
#=============== Bayesian Learning ======================



pdag = iamb(learning.test)
pdag
dag = pdag2dag(pdag, ordering = c("A", "B", "C", "D", "E", "F"))
bn.hc

arcs(bn.hc) # display edges in a Graph object - Baysian Network object

plot(bn.hc)
# compute the score of the Baysian Network
score(set.arc(bn.hc, from = "E", to = "B"), learning.test)
# fitting the parameters of the local distributions, CP table
fit = bn.fit(dag, learning.test)
fit$D
bn.fit.barchart(fit$D)
bn.fit.dotplot(fit$D)



bn.ab = gs(learning.test, blacklist = bl2) # learning with white or blacklisting
plot(bn.ab)
modelstring(bn.ab) # relationships among nodes
#== blacklisting with dataframe and matrix ================
bl1 <- data.frame(from=c("A", "B"), to=c("B", "E"))
bl2 <- matrix(c("A", "B", "B", "C"), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to")))

#============== Learning Soccer Network ============
ddf = european_soccer_leagues[,c(1:11,15)]

ddf$League[ddf$League=='bundesliga'] = 'Bundesliga'

counts <- table(ddf$League)
barplot(counts, main='Data Distriburtion among Leagues', axis.lty = 1)

m.edf = as.matrix(edf)
s.edf = scale(m.edf, center=colMeans(m.edf), scale=colMeans(m.edf)) # normalized matrix

df.edf = as.data.frame(s.edf)
colnames(df.edf)[6] = "LUBS"
#===================================================
colnames(df.edf)
col.tie = c(5,7)
col.notie = c(6,8)
df.edf.tie <- df.edf[, -col.notie] 

bn.soc <- hc(df.edf.tie)
bn.soc
plot(bn.soc)

modelstring(bn.soc) #read modelstring

#========= building a network from model string ===================

bn.soc.1 = model2network("[MHG][MAG|MHG][LWLSS][LMV|MHG:MAG][LWS|MHG:MAG][LLS|MHG:LWS][LUBS|LMV:LWS:LLS][AggregatedAttendance|LMV:LWS:LUBS][LMD|LUBS:LLS:LWLSS]")
bn.soc.no_tie = model2network("[MHG][MAG|MHG][LMV|MHG:MAG][LWS|MHG:MAG][AggregatedAttendance|LMV:LWS]")
bn.soc.tie = model2network("[MHG][MAG|MHG][LUBS|MHG][LMV|MHG:MAG][LWLSS|LMV:LUBS][AggregatedAttendance|LMV:LWLSS:LUBS]")
scor = score(bn.soc.tie, df.edf[,-c(4,5,7)], type= "bic-g") 
scor

#============ fit parameters of a baysian network =========================
fit.soc = bn.fit(bn.soc, df.edf, method = "bayes")
fit.soc
fit.soc$LWS
bn.fit.xyplot(fit.soc$AggregatedAttendance)
bn.fit.qqplot(fit.soc$AggregatedAttendance)
bn.fit.histogram(fit.soc)

coefficients(fit.soc$AggregatedAttendance)

score(bn.soc, df.edf, type="loglik-g") # score a model
choose.direction(bn.soc, data = df.edf, c("MAG", "MHG"), criterion = "bic-g", debug = TRUE)
 # condictional independence test
ci.test("MAG", "MHG", data=df.edf, test = "smc-cor")

#========== How to read a file of choice ================
file.choose() # which opens up the file choose dailogure
#========================================================

