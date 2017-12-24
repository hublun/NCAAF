

#x1 = unlist(coda.Samples[,3])


x1 = post$delta # stanfit object interface
x1
dat = with(density(x1), data.frame(x,y))


cent = dat$x[dat$y == max(dat$y)]

HDIInterval = hdi(density(x1), credMass = 0.95)
HDIInterval

Hdl = HDIInterval[1]
Hdu = HDIInterval[2]

ggplot(data=dat, mapping=aes(x=x,y=y)) + theme_Posterior + 
  
  geom_line() +
  geom_area(mapping = aes(x=x, y = ifelse(x >= Hdl & x <= Hdu, y, 0)), alpha=0.2) +
  geom_vline(xintercept = cent, linetype=2)+
  geom_vline(xintercept = 0, linetype=4, size=1)+  
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(name = "Most Away Goals",
                     #expand = c(0,0), # no expansion buffer 
                     breaks = seq(0, 4, 0.25),
                     limits=c(min(Hdl - 0.5,0), Hdu + 0.5)
                     )
 p1 = last_plot()
 p2 = last_plot()
 p3 = last_plot()
 
 #==================Create Own Theme ======================  
theme_Posterior = theme(
  axis.line.x = element_line(arrow=arrow(length=unit(0.1, "cm")), lineend = "butt"),
  panel.background = element_rect(fill="transparent"), 
  panel.border = element_rect(fill="transparent"),
  plot.background = element_rect(fill = "transparent"),
  #panel.spacing.y = unit(1,"lines"),
  plot.margin = unit(c(1,1,1,1), "cm"), 
  #
  axis.title.y = element_blank(), 
  axis.text.y = element_blank(), axis.ticks.y = element_blank()
)

#===================================Data and Poisson and Normal curves=======
MAG_Plot <- ggplot(data=HWS, aes(x = yG)) + 
  geom_histogram(
                  breaks=seq(-0.5, 10.5, by =1),   alpha = 0.2, aes(y=..density..) # control y scale
                ) + 
  stat_function(geom="bar" ,alpha=0.8, fun = dpois, args=list(lambda=median(HWS$yG)), xlim=c(0,10))+
  stat_function(fun = dnorm, args = list( mean=mean(HWS$yG, na.rm = TRUE), sd=sd(HWS$yG, na.rm=TRUE))) +

  scale_x_continuous(name = "Most Away Goals at the Season level",
                     breaks = seq(0, 10, 1),
                     limits=c(-0.5, 10)) + 
  
 labs(y="Density") + theme_bw()

MHG_Plot <- 
  ggplot(data=HWS, aes(x = yH)) + 
  geom_histogram(
    breaks=seq(-0.5, 10.5, by =1),   alpha = 0.2, aes(y=..density..) # control y scale
  ) + 
  stat_function(geom="bar" ,alpha=0.8, fun = dpois, args=list(lambda=median(HWS$yH)), xlim=c(0,10))+
  stat_function(fun = dnorm, args = list( mean=mean(HWS$yH, na.rm = TRUE), sd=sd(HWS$yH, na.rm=TRUE))) +
  
  scale_x_continuous(name = "Most Home Goals at the Season level",
                     breaks = seq(0, 10, 1),
                     limits=c(-0.5, 10)) + 
  
  labs(y="Density") + theme_bw()





     #theme_bw(plot.title = element_text(hjust = 0.3)) # center the title
#==============================================================
x1=seq(-3, 3, by=0.1)
x1
y1 = dnorm(x1, 1,1)
y1
df = data.frame(x1, y1)

ggplot(data = df, aes(x=x1, y=y1)) + 
  stat_function( geom = "bar", fun=dpois, args = list(lambda=1), xlim=c(-2,8)) + 
  stat_function( geom="line", fun=dnorm, args = list(1,1)) +
  #geom_text(x=5, y=0.2, label="Line: Normal Distribution", size=3) +  
  #geom_text(x=5, y=0.16, label="Bars: Poisson Distribution", size=3) +
  geom_area(aes(x = ifelse(x1<0, x1, 0)), alpha=0.3)+
  
  scale_x_continuous(name = "Count Value", breaks = seq(-3, 10, 1), limits=c(-3.5, 6)) + #labs(y="Density") + 
  scale_y_continuous(name = "Density / Frequency", breaks = seq(0, 0.4, 0.1), limits=c(0, 0.4)) + #labs(y="Density") +   
  theme_bw()


 


#=======================================multiplot ================================= 
multiplot(p1, p2, cols=1)


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
