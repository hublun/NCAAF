library(plyr)

ggplot() +    
  stat_function(aes(x=seq(0, 10, 0.1)), geom="area" ,alpha=0.2, fun = dgamma, args=list(shape = 3, rate = 0.6))

clus = ddply(HWS, .(Club), summarise, avg = mean(yH))
ggplot(data=clus, aes(x=clus$avg)) + geom_density() + stat_function(geom="area" ,alpha=0.2, fun = dgamma, args=list(shape = 18, rate = 5))
+theme_light()


ggplot(data = PBS1000, aes(x=Bias, y=Accuracy)) + 
  geom_hline(yintercept = 0.9466, linetype = 5) +
  geom_hline(yintercept = 0.9548, linetype = 5) +  
  geom_hline(yintercept = 0.9507, linetype = 5) + 
  geom_hline(yintercept = 0.9489, linetype = 5) + 
  geom_hline(yintercept = 0.9565, linetype = 5) + 

  geom_vline(xintercept = 0.959, linetype = 5) + 
  geom_vline(xintercept = 1.95, linetype = 5) + 
  geom_vline(xintercept = 1.535, linetype = 5) + 
  geom_vline(xintercept = 1.627, linetype = 5) + 
  geom_vline(xintercept = 1.44, linetype = 5) + 
  
  geom_point(color = "blue", size=5) + theme_gray()
