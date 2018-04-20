data{

  int<lower=0> N; // lower bound is 0
  int yH[N];
  int yG[N];
 
}

parameters {
  real<lower=0> lambda_h;
  real<lower=0> lambda_g;  
}

transformed parameters{
real delta;
delta = lambda_h - lambda_g;
}

model{

  lambda_h ~ cauchy(0, 3); // prior for lambda
  lambda_g ~ cauchy(0, 2);
  
  yH ~ poisson(lambda_h);
  yG ~ poisson(lambda_g);  
}

generated quantities {
  vector[N] diff_rep;
  
  for (s in 1:N){
    diff_rep[s] = poisson_rng(lambda_h) - poisson_rng(lambda_g);    
  
  }

}

