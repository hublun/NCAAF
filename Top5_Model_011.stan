data{
  int<lower = 1> N;// lower bound is 0
  int<lower = 1> Nc; // number of first levels
 
  
  int xC[N]; // team identifier
 
  int yH[N]; 
  int yG[N];
}

parameters {
  
  real<lower=0> lambda_h[Nc]; // scoring rate differential from between teams
  real<lower=0> lambda_g[Nc];

}

transformed parameters{

 
  real deltalow[Nc]; // rate differential at club level
 


for (i in 1:Nc){

  deltalow[i] =   lambda_h[i] -   lambda_g[i];
 }
}

model{

// priors

 lambda_h ~ gamma(2, 0.1);
 lambda_g ~ gamma(2, 0.1);

// likelihood 

for (i in 1:N){

  yH[i] ~ poisson(lambda_h[xC[i]]);
  yG[i] ~ poisson(lambda_g[xC[i]]);
  }

}


generated quantities {

vector[N] ydiff_rep;
  
  for (s in 1:N){
    ydiff_rep[s] = poisson_rng(lambda_h[xC[s]]) - poisson_rng(lambda_g[xC[s]]);    
  
  }
}

