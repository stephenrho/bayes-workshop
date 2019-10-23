### ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
### brms - example 2
### This example uses simulated data (see sim-brms-ex2.R)
### from a hypothetical memory study where two groups of 
### participants (id column) recalled different items
### under different conditions (A, B - repeated measures)
### acc = accuracy of recall (1 = correct, 0 = incorrect)
### 
### In the first model we don't model items effects.
### In model 2 we do. 
### This example demonstrates another brms model family 
### and extracting fitted values from model objects to perform
### contrasts on both transformed (logit) and manifest 
### (probability correct) scales
### ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

library(brms)

# speed things up
options(mc.cores=parallel::detectCores())

# read the data
dat <- read.csv("examples/accuracy-data.csv")

dat = within(dat, {
  id = as.factor(id)
  item = as.factor(item)
  condition = as.factor(condition)
  group = as.factor(group)
})

# this line changes the way that factors are coded
options(contrasts=c("contr.sum", "contr.sum"))
# using this approach the coefficients sum-to-zero
contrasts(dat$group)
contrasts(dat$condition)

## bad approach ----
# let's start with a bad, but standard, analysis

# starts by aggregating to get proportion correct
dat_agg = aggregate(acc ~ id + condition + group, data = dat, FUN = sum)
dat_agg$p_acc = dat_agg$acc/26

# anova
summary(aov(p_acc ~ condition*group + Error(id/condition), data = dat_agg))

# normal lme 
m0 = brm(p_acc ~ group*condition + (1 + condition | id), 
         data = dat_agg)

pp_check(m0, nsamples=100) # little to do with the fact that we didn't specify priors...

# marginal_effects(m0)

## better approach ----

# model 1: we are going to ignore item variation

get_prior(acc ~ group*condition + (1 + condition | id), data = dat, 
          family = bernoulli(link = "logit"))

priors = c(set_prior("normal(1, 2)", class = "Intercept"),
           set_prior("normal(0, 1)", class = "b"),
           set_prior("cauchy(0, 2.5)", class = "sd"),
           set_prior("lkj(1)", class = "cor"))

## explanation of prior choice ### ### ### ### ### ### 
# coefficients are modeled on the logit scale (logit(p) = log(p/(1-p)))
# which has an effective range of ~-4 to 4 (see plogis(c(-4, 4)) for implied probabilities)
# the Intercept is coded to reflect the grand mean (we're using sum-to-zero coding)
# so normal(1,2) reflects a prior expectation of ~73% accuracy but is fairly broad around that
# for fixed effects ("b") the normal(0,1) prior allows effects to go either direction
# and does not rule out effects of around 2 logits (fairly broad)
# the half-cauchy(2.5) prior on random effect sd is broad for logit scale 
# (see curve(dcauchy(x = x, scale = 2.5), from=0, to=10) )
# finally, lkj(1) prior on correlation matrix is a uniform prior on correlations of -1 to 1
### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# fit the model
m1 = brm(acc ~ group*condition + (1 + condition | id), 
         data = dat, family = bernoulli(link = "logit"), # link could be "probit" - see ?brmsfamily for more detail - would have to use pnorm() where plogis() is used below
         prior=priors)

## an alternative way to fit the model ignoring item variability...
## fit the model to the aggregated data where 'acc' is number correct
# m1.2 = brm(acc | trials(26) ~ group*condition + (1 + condition | id), 
#          data = dat_agg, # note we're using the aggregated data
#          family = binomial(link = "logit"), # note that brms has a separate 'binomial' family
#          prior=priors)

pp_check(m1, nsamples = 100)

marginal_effects(m1, method = "fitted", robust = F)

summary(m1)

# add variation by item...
m2 = brm(acc ~ group*condition + (1 + condition | id) + (1 | item), 
         data = dat, family = bernoulli(link = "logit"),
         prior=priors, 
         sample_prior = "yes")

pp_check(m2, nsamples=100)

summary(m2)

## compare models ----
m1 = add_criterion(m1, c("loo", "waic"))#, "marglik"))
m2 = add_criterion(m2, c("loo", "waic"))#, "marglik"))

loo_compare(m1, m2)
# in this case the loo difference is not convincing either way
# here we'll err on the side of caution and look at results of m2

# plot random effects
stanplot(m2, pars = 'r_item')
stanplot(m2, pars = 'r_item', type = "dens_overlay") # see 'type' for more options
stanplot(m2, pars = paste0("r_id[", 1:40, ",Intercept]"), exact_match = T) # can also use regular expressions for 'pars' (in which case exact_match=F)
stanplot(m2, pars = paste0("r_id[", 1:40, ",condition1]"), exact_match = T)

# convert population level parameters to condition/ group means
newd = expand.grid(condition=c("A", "B"), group=c(1,2))

# extract fitted values
m2_logit = posterior_linpred(m2, newdata = newd, 
                             re_formula = NA, # don't include random effects
                             transform = F) # gives fitted values on logit scale

colnames(m2_logit) = with(newd, paste(condition, group, sep="_"))
head(m2_logit) # so we know which column is which...

# some contrasts on the logit scale
hist(m2_logit[,1] - m2_logit[,2], main="Condition A - B: Group 1", xlab="A - B (logit)")
hist(m2_logit[,3] - m2_logit[,4], main="Condition A - B: Group 2", xlab="A - B (logit)")
hist((m2_logit[,1] - m2_logit[,2]) - (m2_logit[,3] - m2_logit[,4]), 
     main="Condition A - B: Group 1 vs 2", xlab="A - B (logit)")

logit_group_diff = (m2_logit[,1] + m2_logit[,2])/2 - (m2_logit[,3] + m2_logit[,4])/2
hist(logit_group_diff, 
     main="Group 1 vs 2", xlab="A - B (logit)")

quantile(logit_group_diff, probs = c(.025, .5, .975)) # median and 95% CI

mean(logit_group_diff < 0) # proportion of the posterior below 0

# contrasts on the observed (manifest) scale (p = probability correct)
m2_p = plogis(m2_logit)
hist(m2_p[,1] - m2_p[,2], main="Condition A - B: Group 1", xlab="A - B (p)")
hist(m2_p[,3] - m2_p[,4], main="Condition A - B: Group 2", xlab="A - B (p)")
hist((m2_p[,1] - m2_p[,2]) - (m2_p[,3] - m2_p[,4]), 
     main="Condition A - B: Group 1 vs 2", xlab="A - B (p)")

p_group_diff = plogis((m2_logit[,1] + m2_logit[,2])/2) - plogis((m2_logit[,3] + m2_logit[,4])/2)
hist(p_group_diff, 
     main="Group 1 vs 2", xlab="A - B (p)")

quantile(p_group_diff, probs = c(.025, .5, .975)) # median and 95% CI

mean(p_group_diff < 0) # proportion of the posterior below 0

# savage-dickey test for group:condition interaction (requires sample_prior = "yes")
(hy = hypothesis(m2, "group1:condition1 = 0"))

hy_p=plot(hy)[[1]]
# zoom in
hy_p + ggplot2::coord_cartesian(xlim = c(-.2, .2))

# uncomment line below to save objects
# save.image("examples/brms-example2.RData")

