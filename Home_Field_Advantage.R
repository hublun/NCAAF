#================== Home Field Advantage ==========================
#remove()
#============= prepare the working environment ====================
detectCores() # paralell computing

# Check that required packages are installed:
want = c("parallel","rjags","runjags","compute.es")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }

# Load rjags. Assumes JAGS is already installed.
try( library(rjags) )
# Load runjags. Assumes JAGS is already installed.
try( library(runjags) )
try( runjags.options( inits.warning=FALSE , rng.warning=FALSE ) )

# set default number of chains and parallelness for MCMC:
library(parallel) # for detectCores().
nCores = detectCores() 
if ( !is.finite(nCores) ) { nCores = 1 } 
if ( nCores > 4 ) { 
  nChainsDefault = 4  # because JAGS has only 4 rng's.
  runjagsMethodDefault = "parallel"
}
if ( nCores == 4 ) { 
  nChainsDefault = 3  # save 1 core for other processes.
  runjagsMethodDefault = "parallel"
}
if ( nCores < 4 ) { 
  nChainsDefault = 3 
  runjagsMethodDefault = "rjags" # NOT parallel
}

runjagsMethodDefault

#==================  Prerpare Data ===================================
HWS <- ET5[,c(2,3,6,7)]
head(HWS)

HWS$xL = factor(HWS$LID, levels = unique(HWS$LID), labels = c("1","2","3","4","5"))
HWS$League <- toupper(HWS$League)
HWS$xC = factor(HWS$CID, levels = unique(HWS$CID), labels=1:98)       
HWS$xS = factor(HWS$Season, levels=unique(HWS$Season), labels = 1:16)

HWS$yH = as.integer(HWS$MostHomeGoals)
HWS$yG = as.integer(HWS$MostAwayGoals)


hist(HWS$yH)
hist(HWS$yG)

#hist(HWS$yDiff[HWS$xS==5])
#HWS$League[HWS$xL==2]
#============================= Simple JAGS Poisson Model ====================================
y1 = HWS$yH
y2 = HWS$yG

#grp1 = as.integer(HWS$xC)
NTotal = length(y1)
NTotal
meanY1 = mean(y1)
meanY1
meanY2 = mean(y2)
meanY2
sdY1 = sd(y1)
sdY2 = sd(y2)

NLevel = nlevels(HWS$xC)
NLevel
x1 =as.integer(levels(HWS$xC))[HWS$xC]
x1
t.test(y1, y2)
#t.test(y3, mu=0) # t-test show significance of goal differential 

#dy = density(y1)
#y1 = dy$y
#Ntotal = length(y1)

dataList = list(y1=y1, y2=y2, x = x1, Nt = Ntotal, Nl = NLevel)#, m1=meanY1, m2 = meanY2, sd1=sdY1, sd2=sdY2) # data to be fed into JAGS model
# ==+++++++++++++++++++++++ Model definition +++++++++++++++++======
 

 
 cat("model {
 for (i in 1:Nt){
  y1[i]~dpois(mu1[x[i]]) # likelihood distribution
  y2[i]~dpois(mu2[x[i]])
 }
for ( j in 1:Nl){
 mu1[j] ~ dunif(0, 10) #prior distribution for mu1
 mu2[j] ~ dunif(0, 30) #prior distribution for mu2
}
 # transformed parameters

  # scoring rage difference
     for (j in 1:Nl){ mud[j]=mu1[j]-mu2[j]}
 }", 
 file = "model_poisson.jag") # write the model string to file
    
#-----------------------------------------------------------------
 cat("model {
 for (i in 1:Nt){
     y1[i] ~ dnorm(mu1, 1/4) # likelihood distribution
     y2[i] ~ dnorm(mu2, 1/4)
    }
    mu1 ~ dunif(0,30) #prior distribution for mu1
    mu2 ~ dunif(0,30) #prior distribution for mu2
    #sigma1 ~ dunif(1, 15)
    #sigma2 ~ dunif(1, 15)
    # transformed parameters
    
    mud = mu1 - mu2 # scoring rage difference
    
    }", file = "model_gaussian.jag") # write the model string to file

#+++++++++++++++++++ make initial values for parameters in model ++++++++++++++++++++++++
initsList = function(){
  resampledY1 = sample(y1, replace = TRUE)
  resampledY2 = sample(y2, replace = TRUE)
  mu1Init = sum(resampledY1) / length(resampledY1) # mean of y1 resample
  mu2Init = sum(resampledY2) / length(resampledY1) # mean of y2 resample
  mu1Init = 0.001 + 0.028*mu1Init
  mu2Init = 0.001 + 0.028*mu2Init
  #sigmaInit = (sd(y1)+sd(y2))/2
  return (list(mu1 = mu1Init, mu2=mu2Init))
}
#========================================= Run JAGs Model =================================================
jagsModel = jags.model(file = "model_poisson.jag", data=dataList, #inits = initsList, 
                       n.chains = 3, n.adapt = 500) # compile the model and find the appropriate MCMC sampler 
remove(jagsModel)

update(jagsModel, n.iter = 500) #run 'burn-in' cycles of 500

coda.Samples = coda.samples(jagsModel, variable.names = c("mud"), # "sigma1", "sigma2"), 
                            n.iter = 3334) # formal model run and return a list of matrices
#==============================================================================++++++++++++++++++++========
varnames(coda.Samples) # all par names in the coda object

diagMCMC(codaObject = coda.Samples, parName = c("mu1[2]"))
diagMCMC(codaObject = coda.Samples, parName = c("mu2[2]"))

diagMCMC(codaObject = coda.Samples, parName = c("mud[2]"))

#unlist(coda.Samples[3]) # list of three  mcmc.list objects
#====================== plotting multiple panel style====================================================================
par(mfrow=c(1,1))
par(mar=c(2,2,2,2))
par()

median(HWS$yH)
mean(HWS$yH)
hist(HWS$yG, main = "Poisson and Normal Representations of MAG Data",
     probability = TRUE,
     breaks = c(-0.5, 0.5, 1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5),
     xlim=c(-3.5, 10),
     ylim=c(0, 0.25),
     col="#cdcdee",
     border = "#eeeeff",
     ylab="",
     xlab="",
     yaxt="n",
     xaxt="n"
     )



poi = dpois(0:10, 3)

for (i in 1:10){
  lines(c(i-1,i-1),c(0,poi[i]),lwd=10, lty=1, lend = 1 , col="#8899aa")
}


lines(seq(-6.5,9.5, length=800), dnorm(seq(-6.5,9.5, length=800), mean = 2.6, sd=1.67),
      col="#234589",
      lty=1,
      lwd=2)


axis(1, at=c(seq(-5,10,0.5)),
     #labels=y,
     pos = c(-0.001,0.0),
     col="#123466",
     tck = -0.005,
     col.axis="#113377", 
     las=1)
#============================ plot normal curve superimposed on poisson distribution =============
plot(0:16,dpois(0:16, lambda = 1),
     main="Poisson Distribution + Normal Distribution Curve",
     cex=2,
     col="#aabbdd",
     xlab="x",
     ylab="Density(x)",
     type = "h", 
     yaxt="n",
     #mgp=c(2,0.0,-1.5),
     xaxt = "n",
     frame.plot = F, # frame show or hide
     pch = 22,
     lty=1,
     lend=1,
     lwd=15, 
       
     xlim=c(-5,8),
     ylim=c(0,0.39))

lines(seq(-5,7.5, length=800), dnorm(seq(-5,7.5, length=800), mean = 1, sd=1),
      col="#234589",
      type = "l",
      pch=20,
      lty=3,
      lwd=3,
      ljoin=0
      )



lines(c(1,1),c(0,0.45), lty=5)
#abline(v=2.5)

axis(2, at = c(seq(0,0.4, 0.1)),
     #labels=y,
     pos = c(-5.0,-0.02),
     col="#123466",
     tck = -0.005,
     col.axis="#113377", 
     las=1)

axis(1, at=c(seq(-5,8,0.5)),
     #labels=y,
     pos = c(-0.001,0.0),
     col="#123466",
     tck = -0.005,
     col.axis="#113377", 
     las=1)
#===============================================================
par(mfrow=c(1,1))
 
plotPost(coda.Samples[,i], main = "Goal Scoring Rate - Home", xlab=bquote(lambda), cenTend = "mode", xlim=c(1.5, 4.5),
         compVal = 3,
         
         col = "#123498",
         #ROPE = c(2.15,3.15), 
         credMass = 0.90, 
         showCurve = TRUE, 
         border = "green")
 

plotPost(coda.Samples[,2], main = "Goal Scoring Rate - Away", 
         
         col = "#123498",
         xlab=bquote(lambda), cenTend = "mode", xlim=c(-1.5, 4.5),
         compVal = 0.0, 
         #ROPE = c(2.15,3.15), 
         credMass = 0.90, showCurve = TRUE, border = "green")

plotPost(coda.Samples[,3], 
         
         main = "Goal Scoring Differential: Home-Away", 
         xlab=bquote(Delta), 
         #ylab = "Density",
         #ylim=c(0.0, 2.0),
         cenTend = "mean", 
         xlim=c(-1.5, 2.5),
         compVal = 0.0, #ROPE = c(-0.01,.01), 
         credMass = 0.90, 
         HDItextPlace = 0.5,
         showCurve = TRUE, 
         border = "green")

#=======================================================================================================================
#hist(HWDiff)
#length(HWDiff)
#plot(muvalue1, muvalue2, xlab="Most Home Goal Scoring", ylab="Most Away Goal Scoring") # scatter plot
#plot(sort(HWDiff))
#median(HWDiff)
#mean(HWDiff)
#ceiling(HWDiff) # nearest smaller int
#floor(HWDiff) # nearest larger int
#HDIofMCMC(HWDiff, credMass = 0.76) # 0 falls within 95% HDI
#================================================================================================