// normal model with random intercept term
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
  real<lower=0> tau;          // random effect
  vector[S] b;
}
model {
  // priors
  tau ~ cauchy(0,100);
  sigma ~ cauchy(0,50);
  beta[1] ~ normal(300,100);
  beta[2] ~ normal(0,50);
  
  for (s in 1:S){
    b[s] ~ normal(0,tau);
  }
  // likelihood
  for (i in 1:N){
    y[i] ~ normal(x[i] * beta + b[id[i]], sigma);
  }
  
}
generated quantities {
  vector[N] log_lik;      // log likelihood matrix
  vector[N] y_rep;        // posterior predictions
  for (i in 1:N){
    log_lik[i] = normal_lpdf(y[i] | x[i] * beta + b[id[i]], sigma);
    y_rep[i] = normal_rng(x[i] * beta + b[id[i]], sigma);
  }
}
