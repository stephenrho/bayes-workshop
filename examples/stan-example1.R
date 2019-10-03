### ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
### stan - example 1
### Simple example model in Stan using the sleepstudy
### dataset also used in the first brms example
### ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

library(rstan)
library(loo)
library(bayesplot)

sleepstudy <- read.csv("examples/sleepstudy.csv")
sleepstudy$Subject = as.factor(sleepstudy$Subject)

rstan_options(auto_write = TRUE)

# put data in a list
data_list = list(
  N = nrow(sleepstudy),
  y = sleepstudy$Reaction,
  x = model.matrix(~ Days, data = sleepstudy),
  S = length(unique(sleepstudy$Subject)),
  id = as.numeric(sleepstudy$Subject)
)

## fit models ----
# model 1
m1_fit <- stan(
  file = "examples/stan-models/example1-m1.stan",
  data = data_list,
  chains = 4,
  warmup = 1000,
  iter = 2000,
  cores = 4
)

plot(m1_fit, pars="beta")
plot(m1_fit, pars="sigma")

(fit_summary_1 = summary(m1_fit))

# check r-hats
all(fit_summary_1$summary[,"Rhat"] < 1.1, na.rm = T) # or rhat(m1_fit)
max(fit_summary_1$summary[,"Rhat"], na.rm = T)

traceplot(m1_fit, pars="beta")

# model 2
m2_fit <- stan(
  file = "examples/stan-models/example1-m2.stan",
  data = data_list,
  chains = 4,
  warmup = 1000,
  iter = 2000,
  cores = 4
)

plot(m2_fit, pars="beta")
plot(m2_fit, pars="sigma")
plot(m2_fit, pars="tau")
plot(m2_fit, pars="b")

(fit_summary_2 = summary(m2_fit))

# check r-hats
all(fit_summary_2$summary[,"Rhat"] < 1.1, na.rm = T)
max(fit_summary_2$summary[,"Rhat"], na.rm = T)

traceplot(m2_fit)

# model 3
m3_fit <- stan(
  file = "examples/stan-models/example1-m3.stan",
  data = data_list,
  chains = 4,
  warmup = 1000,
  iter = 2000,
  cores = 4
)

plot(m3_fit, pars="beta")
plot(m3_fit, pars="sigma")
plot(m3_fit, pars="tau")
plot(m3_fit, pars="b")

(fit_summary_3 = summary(m3_fit))

# check r-hats
all(fit_summary_3$summary[,"Rhat"] < 1.1, na.rm = T)
max(fit_summary_3$summary[,"Rhat"], na.rm = T)

traceplot(m3_fit, pars=c("beta", "sigma", "tau"))

## compare models -----

# calculate loo
log_lik_1 = extract_log_lik(m1_fit, merge_chains = F)
r_eff_1 = relative_eff(exp(log_lik_1)) # effective sample size divided by total sample size (using this makes loo more accurate)
loo_1 = loo(log_lik_1, r_eff = r_eff_1)

log_lik_2 = extract_log_lik(m2_fit, merge_chains = F)
r_eff_2 = relative_eff(exp(log_lik_2))
loo_2 = loo(log_lik_2, r_eff = r_eff_2)

log_lik_3 = extract_log_lik(m3_fit, merge_chains = F)
r_eff_3 = relative_eff(exp(log_lik_3))
loo_3 = loo(log_lik_3, r_eff = r_eff_3)

loo_compare(loo_1, loo_2, loo_3)

# marginal likelihoods for bayes factors
ml_1 = bridge_sampler(m1_fit)
ml_2 = bridge_sampler(m2_fit)
ml_3 = bridge_sampler(m3_fit)

bayes_factor(ml_3, ml_2)

post_prob(ml_1, ml_2, ml_3)


## explore model 3 posterior -----
# posterior predictive distribution
yrep <- extract(m3_fit, pars = "y_rep")[[1]]

ppc_stat(sleepstudy$Reaction, yrep, stat = "mean")

ppc_dens_overlay(sleepstudy$Reaction, yrep[sample(nrow(yrep), size = 100),])


# posterior distribution for population effect of days
beta <- extract(m3_fit, pars = "beta")[[1]]

hist(beta[,2], breaks = 30, border = F, col="lightblue", main="", xlab="", ylab="", axes=F)
axis(1)
lines(x = quantile(beta[,2], probs = c(.025, .975)), y=c(0,0), lwd=3)
text(10, 50, labels = sprintf("%.2f [%.2f, %.2f]", mean(beta[,2]), 
                              quantile(beta[,2], probs = c(.025)), 
                              quantile(beta[,2], probs = c(.975))))




