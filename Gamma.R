library(plyr)

ggplot() +    
  stat_function(aes(x=seq(0, 10, 0.1)), geom="area" ,alpha=0.2, fun = dgamma, args=list(shape = 3, rate = 0.6))

clus = ddply(HWS, .(Club), summarise, avg = mean(yH))
ggplot(data=clus, aes(x=clus$avg)) + geom_density() + stat_function(geom="area" ,alpha=0.2, fun = dgamma, args=list(shape = 18, rate = 5))
+theme_light()


ggplot(data = PBS1000_1_, aes(x=Bias, y=Accuracy, label=Lab)) + 
  geom_hline(yintercept = 0.95022, linetype = 5) +
  geom_hline(yintercept = 0.9548, linetype = 5) +  
  geom_hline(yintercept = 0.9507, linetype = 5) + 
  geom_hline(yintercept = 0.943, linetype = 5) + 
  geom_hline(yintercept = 0.9566, linetype = 5) +
  
  geom_vline(xintercept = 0.959, linetype = 5) + 
  geom_vline(xintercept = 0.31, linetype = 5) + 
  geom_vline(xintercept = 1.535, linetype = 5) + 
  geom_vline(xintercept = 1.82, linetype = 5) + 
  geom_vline(xintercept = 1.44, linetype = 5) + 
  geom_point(color="#336655", size=4) + theme_gray()+
  geom_text(color="#123321", size=5, hjust = -0.5, vjust = -0.5)
