#=========== plot a quadratic function +++++++========
x = seq(-2,2,0.1)
y = x^2
plot(x, y, type = "b")
plot(x=1:4, y=c(1,2,3,4), type="o")
#================================
ABC = c("A","B","C")
rep(ABC, 2)
rep(ABC, times=c(4,2,1))
rep(ABC, each=2, length=10)
#===============================
a = array(1:24, dim=c(3,4,2), dimnames=list(RowDimName = c("R1", "R2", "R3"),
                                        ColDimName = c("C1","C2","C3", "C4"),
                                        LayDimName = c("L1", "L2")
                                        ))
a["R3",,"L2"]
#============Simulating CoinFlipping==================
N= 500 
pHeads = 0.5
flipSeq = sample(x=c(0,1), prob = c(1-pHeads, pHeads), size=N, replace = TRUE)
r=cumsum(flipSeq)
r
n=1:N
runProp = r/n
plot(n,runProp, type="o", log="x", col="skyblue", 
     xlim=c(1,N), ylim=c(0.0,1.0), cex.axis=1.5,
     xlab="Flip Number", ylab="Proportion Heads", cex.lab=1.5,
     main="Running Proportion of Heads", cex.main=1.5)
abline(h=pHeads, lty="dotted")
flipLetters = paste(c("T","H")[flipSeq[1:10]+1], collapse = "")
displayString = paste0("Flip Sequence = ", flipLetters, "...")
text(N, 0.9, displayString, adj = c(1, 0.5), cex=1.3)
text(N, 0.8, paste("End Proportion = ", runProp[N]), adj = c(1, 0.2), cex=1.0)
#============ Hair and Eye Color ====================
show(HairEyeColor)
str(HairEyeColor)
EyehairFreq = apply(HairEyeColor, c("Eye", "Hair"), sum) # sum across sex
EyehairProp = EyehairFreq / sum(EyehairFreq) # joint proportions
show(round(EyehairProp, 2))
EyeFreq = apply(HairEyeColor, c("Eye"), sum) # sum across hair sex
EyeProp = EyeFreq / sum(EyeFreq)

HairFreq = apply(HairEyeColor, c("Hair"), sum) # sum across eye sex
HairProp = HairFreq / sum(HairFreq)
HairProp
EyehairProp["Brown",] / EyeProp["Brown"] # conditional probability of hair color given eye color
#================= Bimodal Distribution ===================
Theta = seq(0, 1, length=1000)
Theta
rep(1,200)
pTheta = c(rep(1,200), seq(1,100, length=50), seq(100,1,length=50), rep(1,200), rep(1,200),
           seq(1,100, length=50), seq(100,1,length=50), rep(1,200))
pTheta = pTheta/sum(pTheta) # normalizing prior distribution
source(file.choose())
Data =c(rep(0,88), rep(1,99))
posterior = BernGrid(Theta, pTheta, Data, plotType = "Bars", showCentTend = "None",showHDI = FALSE, showpD = FALSE )
#==================
source(file.choose())
t = 0.75 # prior mode
n = 25 # prior effective sample size
a = t*(n-2)+1
b = (1-t)*(n-2)+1
Prior = c(a,b) # Prior as vector of two shape parameters
N = 20
z=17
Data=c(rep(0, N-z), rep(1, z))
openGraph(width = 5, height = 7)
posterior = BernBeta(priorBetaAB = Prior, Data = Data, plotType = "Bars", showCentTend = "Mode", showHDI = TRUE, showpD = FALSE)
saveGraph(file="BernBeta Example", type="jpg")
#====================== end of chapter 6 ============
