
data {
  int<lower=0> N;             // n observations
  vector[N] y;                // reaction times
  matrix[N,2] x;              // design matrix (intercept, days)
  int<lower=0> S;             // n subjects
  int<lower=1,upper=S> id[N]; // subject ids
}
parameters {
  vector[2] beta;             // fixed effects
  real<lower=0> sigma;        // residual SD
  vector<lower=0>[2] tau;     // random effects
  vector[2] z[S];             // used to specify random effects
  cholesky_factor_corr[2] L;  // prior on cholesky factor of correlation matrix
}

transformed parameters {
  vector[2] b[S];           // mean + individual params
  {
    matrix[2,2] Sigma;      // random effects correlation matrix
    Sigma = diag_pre_multiply(tau, L);
    for (i in 1:S){
      b[i] = beta + Sigma * z[i];
    }
  }
}
model {
  // priors
  tau ~ cauchy(0,100);
  sigma ~ cauchy(0,50);
  L ~ lkj_corr_cholesky(1.0);
  beta[1] ~ normal(300,100);
  beta[2] ~ normal(0,50);
  
  for (s in 1:S){
    z[s] ~ normal(0,1);
  }
  // likelihood
  for (i in 1:N){
    y[i] ~ normal(dot_product(x[i,], b[id[i]]), sigma);
  }
  
}
generated quantities {
  vector[N] log_lik;      // log likelihood matrix
  vector[N] y_rep;        // posterior predictions
  for (i in 1:N){
    log_lik[i] = normal_lpdf(y[i] | dot_product(x[i,], b[id[i]]), sigma);
    y_rep[i] = normal_rng(dot_product(x[i,], b[id[i]]), sigma);
  }
}
