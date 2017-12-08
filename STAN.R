rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


modelString ="
data{
  int<lower=0> N; // lower bound is 0
  int y[N];
}

parameters {
  real<lower=0, upper=1> theta;
}

model{
  theta ~ beta(1,1);
  y ~ bernoulli(theta); 
}
"
stanDso = stan_model(model_code = modelString)

N = 50; z=10; y=c(rep(1, z), rep(0, N - z))

dataList = list(y = y, N = N)

stanFit = sampling(object = stanDso, data=dataList, chains=3, iter=1000, warmup=200, thin=1)
traceplot(stanFit)
stanFit



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