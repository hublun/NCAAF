library(plyr)

ggplot() +    
  stat_function(aes(x=seq(0, 10, 0.1)), geom="area" ,alpha=0.2, fun = dgamma, args=list(shape = 3, rate = 0.6))

clus = ddply(HWS, .(Club), summarise, avg = mean(yH))
ggplot(data=clus, aes(x=clus$avg)) + geom_density() + stat_function(geom="area" ,alpha=0.2, fun = dgamma, args=list(shape = 18, rate = 5))
+theme_light()


ggplot(data = Accuracy_vs_Bias, aes(x=Bias, y=Accuracy)) + geom_point(color = "white") + theme_dark()
