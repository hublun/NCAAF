#================= paralell computing multiple core =========
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#============================================================
 
#===========Plot Gamma ================

#===========compile model ========================
stanDso = stan_model("Top5_Model_0.stan") 

sm011 =  stan_model("Top5_Model_011.stan") 

sm11 = stan_model("Top5_Model_11.stan") 

sm2 = stan_model("Top5_Model_2.stan") 
#===============  Data Feed ======================

N=length(HWS$yH)
yh= HWS$yH
yg = HWS$yG
x1 = HWS$xC
x2 = HWS$xL 
xc = as.integer(levels(x1))[x1]
xc
xl = as.integer(levels(x2))[x2]
xl
ydiff = HWS$yH - HWS$yG
ydiff  
nc =  length(unique(xc))
nl =  length(unique(x2))
nl
#=============================================================
dataList0 = list(yH = HWS$yH, yG = HWS$yG, N = length(HWS$yH))

dataList01 = list(hgdiff=ydiff, N = length(HWS$yH))

dl2 = list(y1 = yg, y2 = yh, xL=xc, xH = xl,  
                N = N, Nl = nc, Nh = nl)

dl11 = list(yH = yh, yG = yg, xC = xc, N = length(HWS$yH), Nc=nc)
dl12 = list(yH = yh, yG = yg, xC = xl, N = length(HWS$yH), Nc=nl)


#============================================================
remove(fit11, stanDso)
#========

fit11 = sampling(object = sm011, data=dl12, 
               init=0.1, control=list(adapt_delta = 0.95),
               chains=4, iter=888, warmup=444, thin=1)

class(fit11)

traceplot(fit11)
summary(fit11)

# access and cgange parameter names for display

 

pairs(fit)

# stan plot functions

plot(fit, plotfun="rhat")

plot(fit, plotfun="trace", pars=c("AS_Nancy_Lorraine"))


plot(fit, ci_level = 0.95, point_est ="mean", est_color = "#ffffff",
     
  show_outer_line = TRUE, outer_level = 0.99,
     
  pars=c("AS_Nancy_Lorraine"), 
     
  show_density=TRUE, fill_color="#123489") +
  
    geom_vline(xintercept = 0, linetype=2) + xlab("Goal Scoring Rate Differential (Home - Away)")+ylab("Level") +
  
    scale_x_continuous(#name = label,
                     expand = c(0,0), # no expansion buffer 
      breaks = seq(0, 1.5, 0.2), limits=c(-0.5, 2.0)) +
  
    theme_light()#theme_Posterior



get_posterior_mean(fit0)


post <- extract(fit11)

post$ydiff_rep

tdiff = t(post$ydiff_rep)

dim(tdiff)


#nrow(post$delta_h)
#shinystan for plots and analysis

launch_shinystan(fit11)
#======== convert stan format to coda format ========== 

#================= setting up STAN =================
Sys.which("g++")
Sys.getenv('PATH')
system('g++ -v')
system('where make')
Sys.which('g++')

dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) 
  dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) 
  file.create(M)
cat("\nCXXFLAGS=-O3 -Wno-unused-variable -Wno-unused-function", 
    file = M, sep = "\n", append = TRUE)

fx <- inline::cxxfunction( signature(x = "integer", y = "numeric" ) , '
	return ScalarReal( INTEGER(x)[0] * REAL(y)[0] ) ;
                           ' )
fx( 2L, 5 )

remove.packages("rstan")

remove.packages('Rcpp')
#====================================================