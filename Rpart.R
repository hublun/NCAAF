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
bn.hc
plot(bn.hc)

bn.ab = gs(learning.test, blacklist = c("B", "A"))
plot(bn.ab)




#============== Learning Soccer Network ============
bn.soc <- hc(df.edf)
bn.soc
plot(bn.soc)

