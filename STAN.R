#================= paralell computing multiple core =========
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
#============================================================
simplePoisson ="
  
data{
  int<lower=0> N; // lower bound is 0
 
int y[N]; 
 
}

parameters {
real<lower=0> lambda;
 
}

transformed parameters{}

model{

  lambda ~ uniform(0,25); // prior for lambda
  y ~ poisson(lambda);
  }

"
#===========================
modelPoisson ="

data{
  int<lower=0> N;// lower bound is 0
  int<lower=0> Nl; // number of levels
  int x[N]; // group identifier
  int y1[N]; 
  int y2[N];
}

parameters {
  real<lower=0> lambda1[Nl];
  real<lower=0> lambda2[Nl];
}

transformed parameters{

  real delta[Nl];
  for (i in 1:Nl){
    delta[i] = lambda1[i] - lambda2[i];
 }
}

model{

  lambda1 ~ uniform(0,25); // prior for lambda
  lambda2 ~ uniform(0,25);
for (i in 1:N){
  y1[i] ~ poisson(lambda1[x[i]]);
  y2[i] ~ poisson(lambda2[x[i]]);
  }
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
#stanDso = stan_model(model_code = simplePoisson) 

stanDso = stan_model(model_code = modelPoisson) 
#===============  Data Feed ======================

N=length(HWS$yH)
y1= HWS$yH
y2 = HWS$yG
x = HWS$xL
x 
x1 = as.integer(levels(x))[x]
x1
n1 = nlevels(x)
dataList = list(y1 = y1, y2 = y2, x=x1, 
                N = N, Nl = n1)

fit = sampling(object = stanDso, data=dataList, chains=3, iter=333, warmup=155, thin=1)

class(fit)
traceplot(fit)
summary(fit)

plot(fit, ci_level = 0.95, #pars=c("delta[2]"), 
     
  show_density=TRUE, fill_color="#dedeff") +
  
    geom_vline(xintercept = 0, linetype=2) + xlab("")+ylab("") + 
  
    theme_grey()#theme_Posterior

get_posterior_mean(fit)

post <- extract(fit)

ncol(post$delta)

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