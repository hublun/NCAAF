#================= paralell computing multiple core =========
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#============================================================

modelPoisson ="

data{
  int<lower=0> N; // lower bound is 0
  int y1[N]; 
  int y2[N];
}

parameters {
  real<lower=0> lambda1;
  real<lower=0> lambda2;
}

transformed parameters{

  real delta;
  delta = lambda1 - lambda2;
}

model{
  lambda1 ~ uniform(0,20);
  lambda2 ~ uniform(0,20);
  y1 ~ poisson(lambda1);
  y2 ~ poisson(lambda2);
}"
#-------------------------------------
modelGaussian ="

data{
  int<lower=0> N; // lower bound is 0
  real y1[N]; 
  real y2[N];
}

parameters {
  real<lower=0> mu1;
  real<lower=0> mu2;
}

transformed parameters{

real delta;
delta = mu1 - mu2;
}

model{
  mu1 ~ uniform(0,20);
  mu2 ~ uniform(0,20);
  y1 ~ normal(mu1, 2);
  y2 ~ normal(mu2, 2);
}"

#===========compile model ========================

stanDso = stan_model(model_code = modelGaussian) 

#===============  Data Feed ======================

N=length(HWS$yH)
y1= HWS$yH
y2 = HWS$yG

dataList = list(y1 = y1, y2 = y2, N = N)

fit = sampling(object = stanDso, data=dataList, chains=3, iter=3333, warmup=555, thin=1)

class(fit)
traceplot(stanFit)
plot(fit)
post <- extract(fit)

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