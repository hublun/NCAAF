library(HDInterval)
tdiff[1, ]
hdi(tdiff[1,], credMass=0.95)
ydiff[1]

hdl95 = apply(tdiff, 1, hdi95)
hdl50 = apply(tdiff, 1, hdi50)
dim(hdl50)


hdls = rbind(hdl95,hdl50)
index1 = 1:length(ydiff)
rep =cbind(t(hdls), ydiff)
#============
remove(dff)
#==================================
hdi50 <- function(x){
  return(hdi(x, credMass = 0.5))
}
#==================================

dff <- data.frame(rep)
dff = dff[order(ydiff),]
dff

dff = cbind(dff, index1)
#==================================

ggplot(data=dff, aes(x=index1)) + geom_point(aes(y=ydiff), color="blue", size=1)+
  geom_segment(mapping=aes(xend=index1, y=lower, yend=upper), color="#ddee33", alpha=0.2) +
  geom_segment(mapping=aes(xend=index1, y=lower.1, yend=upper.1), color="#889933", alpha=0.2) + theme_light()
  #geom_line(aes(y=lower.1)) + geom_line(aes(y=upper.1))

#==================================
length(which(dff$ydiff <= dff$upper & dff$ydiff >= dff$lower))
length(which(dff$ydiff <= dff$upper.1 & dff$ydiff >= dff$lower.1))
length(index) 

922/1122
950/1122
1120/1122
931/1122
944/1122
909/1122

colnames(rep)
colnames(rep)[4]
colnames(rep)[4] <- "upper50"
repp <- cbind(HWS$xC, rep)

colnames(repp)
colnames(repp)[1]
colnames(repp)[1] <- "club"

repp <- as.data.frame(repp)
#===========
tail(repp)
aggrepp <- aggregate(repp, list(repp$club), mean)
length(which(aggrepp$ydiff <= aggrepp$upper50 & aggrepp$ydiff >= aggrepp$lower50))

ggplot(data=aggrepp, aes(x=club)) + geom_point(aes(y=ydiff), color="blue", size=1) +
  
geom_segment(mapping=aes(xend=club, y=lower50, yend=upper50), color="#ddee33", alpha=0.8)  + theme_light()
