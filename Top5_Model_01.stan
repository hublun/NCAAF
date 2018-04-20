data{

  int<lower=0> N; // lower bound is 0
  vector[N] hgdiff;
 
}

parameters {
  real delta;
  real sigma;  
}

transformed parameters{
//real delta;
//delta = lambda_h - lambda_g;
}

model{

  delta ~ cauchy(0, 1);
  sigma ~ cauchy(0, 1);
  hgdiff ~ normal(delta, sigma);
  
  
}

generated quantities {
  vector[N] hgdiff_rep;
  
  for (s in 1:N){
    hgdiff_rep[s] = normal_rng(delta, sigma);
  
  }

}

