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

  //real mul;
  //real muh;

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
  
  real delta_league[Nh]; //deviation differential at league level
  real delta_club[Nl]; //deviation differential at club level
 
  delta_top = beta_01 - beta_02;


for (i in 1:N){

  lambda_h1[xH[i]] = beta_01 + dev_high1[xH[i]];
  lambda_h2[xH[i]] = beta_02 + dev_high2[xH[i]];

  delta_h[xH[i]] =   lambda_h1[xH[i]] -   lambda_h2[xH[i]];
  delta_league[xH[i]] =   dev_high1[xH[i]] -   dev_high2[xH[i]];

  lambda_l1[xL[i]] = lambda_h1[xH[i]] + dev_low1[xL[i]];
  lambda_l2[xL[i]] = lambda_h2[xH[i]] + dev_low2[xL[i]]; 

  delta_l[xL[i]] =   lambda_l1[xL[i]] -   lambda_l2[xL[i]];
  delta_club[xL[i]] =   dev_low1[xL[i]] -   dev_low2[xL[i]];
 }

}

model{

 beta_01 ~ cauchy(2, 5);
 beta_02 ~ cauchy(2, 5);


 sigma_high ~ cauchy(0, 0.5);
 sigma_low ~ cauchy(0, 0.5);



 dev_high1 ~ normal(0, sigma_high);
 dev_high2 ~ normal(0, sigma_high);

 dev_low1 ~ normal(0, sigma_low);
 dev_low2 ~ normal(0, sigma_low);


// likelihood 

for (i in 1:N){

  y1[i] ~ poisson(lambda_l1[xL[i]]);
  y2[i] ~ poisson(lambda_l2[xL[i]]);
  }

}

generated quantities {

  int<lower=0> yH_rep[N];
  
  for (n in 1:N)
      yH_rep[n] = poisson_rng(lambda_l2[xL[n]]);
}
