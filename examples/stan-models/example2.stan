
data {
  int<lower=0> N;             // N observations
  int<lower=0> S;             // number of participants
  int<lower=1,upper=S> id[N]; // participant id
  vector[N] p_x;              // presented x coordinate
  vector[N] p_y;              // presented y coordinate
  vector[N] r_x;              // recalled x coordinate
  vector[N] r_y;              // recalled y coordinate
  real<lower=0> A;            // for determining uniform (guess) density
}
parameters {
  real mu_m;                  // population mean for m (logit scale)
  real mu_s;                  // population mean for s (log scale)
  
  real<lower=0> sd_m;         // population sd for m (logit)
  real<lower=0> sd_s;         // population sd for s (log)
  
  vector[S] m;                // individual m parameters (logit)
  vector[S] s;                // individual s parameters (log)
}
model {
  // priors
  sd_m ~ cauchy(0,2.5);
  sd_s ~ cauchy(0,2.5);
  
  mu_m ~ normal(0,5);
  mu_s ~ normal(0,5);
  
  // sample individual parameters from population
  for (i in 1:S){
    m[i] ~ normal(mu_m, sd_m);
    s[i] ~ normal(mu_s, sd_s);
  }
  // likelihood
  for (n in 1:N)
    target += log_mix(inv_logit(m[id[n]]),
                      normal_lpdf(r_x[n] | p_x[n], exp(s[id[n]])) + 
                      normal_lpdf(r_y[n] | p_y[n], exp(s[id[n]])),
                      log(1/A));
}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N)
    log_lik[n] = log_mix(inv_logit(m[id[n]]),
                      normal_lpdf(r_x[n] | p_x[n], exp(s[id[n]])) + 
                      normal_lpdf(r_y[n] | p_y[n], exp(s[id[n]])),
                      log(1/A));
}
