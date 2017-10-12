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

str(learning.test)
bn.hc <- hc(learning.test, score = "aic")
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
bn.soc <- hc(df.edf)
bn.soc
plot(bn.soc)

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

