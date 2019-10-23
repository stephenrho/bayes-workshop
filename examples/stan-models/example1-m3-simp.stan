// model with random intercept and slope (correlated)
// this is a simple version that does not use tricks to speed up sampling
// see example1-m3.stan for a potentially faster version
data {
  int<lower=0> N;             // n observations
  vector[N] y;                // reaction times
  matrix[N,2] x;              // design matrix (intercept, days)
  int<lower=0> S;             // n subjects
  int<lower=1,upper=S> id[N]; // subject ids
}
parameters {
  corr_matrix[2] Sigma;       // correlation matrix (note capital S)
  vector[2] beta;             // fixed effects
  vector[2] b[S];             // individual coefficients
  real<lower=0> sigma;        // residual SD
  vector<lower=0>[2] tau;     // random effects
}
model {
  // priors
  tau ~ cauchy(0,100);
  sigma ~ cauchy(0,50);
  Sigma ~ lkj_corr(1.0);
  beta[1] ~ normal(300,100);
  beta[2] ~ normal(0,50);
  
  for (s in 1:S){
    b[s] ~ multi_normal(beta, quad_form_diag(Sigma, tau));
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
