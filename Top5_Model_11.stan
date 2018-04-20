data{
  int<lower = 1> N;// lower bound is 0
  int<lower = 1> Nc; // number of first levels
 
  
  int xC[N]; // team identifier
 
  int yH[N]; 
  int yG[N];
}

parameters {
  
  real <lower = 0> beta_h;  // intercept
  real <lower = 0> beta_g;  // intercept

  real sigmalow;
 
  real devlow_h[Nc]; // devation from average between teams
  real devlow_g[Nc];

}

transformed parameters{

  real delta_top;
  real<lower=0> lambdalow_h[Nc];
  real<lower=0> lambdalow_g[Nc];
 
  real deltalow[Nc]; // rate differential at club level
 
  delta_top = beta_h - beta_g;


for (i in 1:Nc){

  lambdalow_h[i] = beta_h + devlow_h[i];
  lambdalow_g[i] = beta_g + devlow_g[i]; 

  deltalow[i] =   lambdalow_h[i] -   lambdalow_g[i];

 }

}

model{

 beta_h ~ cauchy(0, 10);
 beta_g ~ cauchy(0, 10);


 sigmalow ~ cauchy(0, 1);

 devlow_g ~ normal(0, sigmalow);
 devlow_h ~ normal(0, sigmalow);


// likelihood 

for (i in 1:N){

  yH[i] ~ poisson(lambdalow_h[xC[i]]);
  yG[i] ~ poisson(lambdalow_g[xC[i]]);
  }

}

generated quantities {

vector[N] ydiff_rep;
  
  for (s in 1:N){
    ydiff_rep[s] = poisson_rng(lambdalow_h[xC[s]]) - poisson_rng(lambdalow_g[xC[s]]);    
  
  }
}

