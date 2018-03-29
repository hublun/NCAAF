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
#=================================================
mPoisson ="

data{
  int<lower = 1> N;// lower bound is 0
  int<lower = 1> Nl; // number of first levels
  int<lower = 1> Nh; // number of higher levels
  
  int xL[N]; // team identifier
  int xH[N]; // league identifier  
  int y1[N]; 
  int y2[N];
}

parameters {
  
  real <lower = 0> beta_01;  // intercept
  real <lower = 0> beta_02;  // intercept

  real <lower = 0> sigma_low;
  real <lower = 0> sigma_high;
  
  real dev_high1[Nh]; // devation from average between leagues
  real dev_high2[Nh];
 
  real dev_low1[Nl]; // devation from average between teams
  real dev_low2[Nl];

}

transformed parameters{

  real delta_top;

  real<lower=0> lambda_h1[Nh];
  real<lower=0> lambda_h2[Nh];
 
  real<lower=0> lambda_l1[Nl];
  real<lower=0> lambda_l2[Nl];

  real delta_h[Nh]; //rate differential at league level
  real delta_l[Nl]; // rate differential at club level
 
  delta_top = beta_01 - beta_02;


for (i in 1:N){

  lambda_h1[xH[i]] = beta_01 + dev_high1[xH[i]];
  lambda_h2[xH[i]] = beta_02 + dev_high2[xH[i]];

  delta_h[xH[i]] =   lambda_h1[xH[i]] -   lambda_h2[xH[i]];

  lambda_l1[xL[i]] = lambda_h1[xH[i]] + dev_low1[xL[i]];
  lambda_l2[xL[i]] = lambda_h2[xH[i]] + dev_low2[xL[i]]; 

  delta_l[xL[i]] =   lambda_l1[xL[i]] -   lambda_l2[xL[i]];

 }

}

model{

beta_01 ~ cauchy(0, 10);
beta_02 ~ cauchy(0, 10);

sigma_high ~ cauchy(0, 5);
sigma_low ~ cauchy(0, 5);



dev_high1 ~ normal(0, sigma_high);
dev_high2 ~ normal(0, sigma_high);

dev_low1 ~ normal(0, sigma_low);
dev_low2 ~ normal(0, sigma_low);


// likelihood 

for (i in 1:N){

  y1[i] ~ poisson(lambda_l1[xL[i]]);
  y2[i] ~ poisson(lambda_l2[xL[i]]);
  }

}"

#===========compile model ========================
#stanDso = stan_model(model_code = simplePoisson) 

stanDso = stan_model(model_code = mPoisson) 
#===============  Data Feed ======================

N=length(HWS$yH)
y1= HWS$yH
y2 = HWS$yG
x1 = HWS$xC
x2 = HWS$xL 
x1 = as.integer(levels(x1))[x1]
x2 = as.integer(levels(x2))[x2]
x2
 
n1 =  length(unique(x1))
n2 =  length(unique(x2))
n2
dataList = list(y1 = y1, y2 = y2, xL=x1, xH = x2,  
                N = N, Nl = n1, Nh = n2)

#========
remove(fit, stanDso)
#========

fit = sampling(object = stanDso, data=dataList, 
               init=0, control=list(adapt_delta = 0.9),
               chains=3, iter=999, warmup=333, thin=1)

class(fit)

traceplot(fit)
summary(fit)

# access and cgange parameter names for display

names(fit)

names(fit)[211]
names(fit)[211] <- "Soccer"

names(fit)[418]
names(fit)[418] <- "La_Liga"

names(fit)[419]
names(fit)[419] <- "Serie_A"

names(fit)[420]
names(fit)[420] <- "Ligue_1"

names(fit)[421]
names(fit)[421] <- "Bundesliga"

names(fit)[422]
names(fit)[422] <- "EPL"

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



plot(fit, ci_level = 0.90, point_est ="mean", est_color = "#126622",
     
      show_outer_line = TRUE, outer_level = 0.95,
     
      pars=c("delta_l[1]", "delta_l[2]", "delta_l[3]", "delta_l[4]", "delta_l[5]", 
            "delta_l[6]", "delta_l[7]", "delta_l[8]", "delta_l[9]", "delta_l[10]",
            "delta_l[11]", "delta_l[12]", "delta_l[13]", "delta_l[14]", "delta_l[15]",
            "delta_l[16]", "delta_l[17]", "delta_l[18]", "delta_l[19]", "delta_l[20]"), 
     
      show_density=FALSE, fill_color="#123489") + geom_vline(xintercept = 0, linetype=2) +
  
      xlab("Goal Scoring Rate Differential (Home - Away)")+ylab("Team") +
  
  scale_x_continuous(#name = label,
    expand = c(0,0), # no expansion buffer 
    breaks = seq(-1, 1.5, 0.2), limits=c(-0.4, 1.6)) +
  
  theme_light()#theme_Posterior








get_posterior_mean(fit)


post <- extract(fit)

#nrow(post$delta_h)
#shinystan for plots and analysis

launch_shinystan(fit)
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