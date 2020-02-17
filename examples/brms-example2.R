### ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
### brms - example 2
### This example uses simulated data (see sim-brms-ex2.R)
### from a hypothetical memory study where two groups of 
### participants (id column) recalled different items
### under different conditions (A, B - repeated measures)
### acc = accuracy of recall (1 = correct, 0 = incorrect)
### 
### In the first model we don't model item effects.
### In model 2 we do. 
### 
### There are questions for you to complete through the
### script. Answers are given at the end.
###
### This example demonstrates another brms model family 
### and extracting fitted values from model objects to perform
### contrasts on both transformed (logit) and manifest 
### (probability correct) scales
### ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

library(brms)
library(bayesplot)

# speed things up
options(mc.cores=parallel::detectCores()) # don't worry if this doesn't work

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

## Fairly common approach ----
# that is less than ideal...
# see https://www.psych.ualberta.ca/~pdixon/Home/resources/Dixon2008.pdf

# starts by aggregating to get proportion correct
dat_agg = aggregate(acc ~ id + condition + group, data = dat, FUN = sum)
dat_agg$p_acc = dat_agg$acc/26

# anova
summary(aov(p_acc ~ condition*group + Error(id/condition), data = dat_agg))

# normal lme
m0 = brm(p_acc ~ group*condition + (1 + condition | id), 
         data = dat_agg)

pp_check(m0, nsamples=100) # doesn't look great
# little to do with the fact that we didn't specify priors...

# marginal_effects(m0)

## Better approach ----
# linear model on the log odds (logit) of correct recall

## Model 1: we are going to ignore item variation ----

# Q 1 ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
# some priors are specified below for model 1
# how would we find out what priors we need for 
# a particular model specification?



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
         prior=priors, 
         save_all_pars=T)

## an alternative way to fit the model ignoring item variability...
## fit the model to the aggregated data where 'acc' is number correct
## uncomment lines below to run
# m1.2 = brm(acc | trials(26) ~ group*condition + (1 + condition | id), 
#          data = dat_agg, # note we're using the aggregated data
#          family = binomial(link = "logit"), # note that brms has a separate 'binomial' family
#          prior=priors)

summary(m1) # coefficients are on the logit scale

# posterior predictive checks
pp_check(m1, nsamples = 100) # note y and yrep are smoothed, hence area over != 0,1
ppc_stat(y = dat$acc, yrep = posterior_predict(m1)) # mean = expected proportion correct
ppc_stat_grouped(y = dat$acc, yrep = posterior_predict(m1), group = dat$id)

# marginal effects (condition medians and 95% CIs on probability scale)
marginal_effects(m1)

## Model 2: include a random intercept for item ----

# Q 2 ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
# We want to build on model 1 by accounting
# for variation in performance attributable to 
# item effects. The model formula isn't complete
# how would you complete it?

m2 = brm(acc ~ group*condition + (1 + condition | id), # <---- change this for Q 2
         data = dat, family = bernoulli(link = "logit"),
         prior=priors, 
         sample_prior = "yes", # for savage-dickey
         save_all_pars = T)

summary(m2)

pp_check(m2, nsamples=100)



## Compare models ----

# Q 3 ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
# to compare the models using loo or Bayes factors
# we need to add some extra stuff. How do you do that?


loo_compare(m1, m2)
m1$loo
m2$loo

# in this case the loo difference is not convincing either way
# here we'll err on the side of caution and look at results of m2

bayes_factor(m1, m2)
# loo and bf won't always agree

## Plot random effects ----
stanplot(m2, pars = 'r_item')
stanplot(m2, pars = 'r_item', type = "dens_overlay") # see 'type' for more options
stanplot(m2, pars = paste0("r_id[", 1:40, ",Intercept]"), exact_match = T) # can also use regular expressions for 'pars' (in which case exact_match=F)
stanplot(m2, pars = paste0("r_id[", 1:40, ",condition1]"), exact_match = T)

## Work with posterior samples ----
# convert population level parameters to condition/ group means
newd = expand.grid(condition=c("A", "B"), group=c(1,2))

# extract fitted values
m2_logit = posterior_linpred(m2, newdata = newd, 
                             re_formula = NA, # don't include random effects
                             transform = F) # gives fitted values on logit scale

colnames(m2_logit) = with(newd, paste(condition, group, sep="_"))
head(m2_logit) # so we know which column is which...

## condition means and 95% CIs converted back to probabilities

apply(plogis(m2_logit), # plogis converts logit to p (aka inverse-logit)
      2, # apply over columns
      function(x) c(mean=mean(x), quantile(x, probs = c(.025,.975)))) # calculate mean and CI

# compare to 
m2_me = marginal_effects(m2) # marginal_effects uses median instead of mean (i.e., robust=T)
m2_me$`group:condition`

## Contrast conditions ----

# function to make histogram with posterior mean + 95% CI
plot_contr = function(x, title="", x_lab="", col="lightblue"){
  # takes samples of a contrast (x; e.g., by subtracting mcmc samples) and plots a histogram
  h=hist(x, breaks = 30, main=title, xlab=x_lab, col=col, border = F, probability = T)
  text(mean(x), max(h$density)*.2, labels = sprintf("Mean + 95%% CI:\n%.2f [%.2f, %.2f]",
                                           mean(x), 
                                           quantile(x, probs = c(.025)), 
                                           quantile(x, probs = c(.975))))
}

# some contrasts on the logit scale
plot_contr(x = m2_logit[,1] - m2_logit[,2], # remember which columns are which...
           title = "Condition A - B: Group 1", 
           x_lab = "A - B (logit)")

plot_contr(x = m2_logit[,3] - m2_logit[,4], 
           title = "Condition A - B: Group 2", 
           x_lab = "A - B (logit)")

# is the difference larger in group 1 vs group 2?
plot_contr(x = (m2_logit[,1] - m2_logit[,2]) - (m2_logit[,3] - m2_logit[,4]), 
           title = "Condition A - B: Group 1 vs 2", 
           x_lab = "A - B (logit)")

# overall age difference 
logit_group_diff = (m2_logit[,1] + m2_logit[,2])/2 - (m2_logit[,3] + m2_logit[,4])/2
plot_contr(x = logit_group_diff, 
           title = "Group 1 vs 2", 
           x_lab = "logit difference")

mean(logit_group_diff < 0) # proportion of the posterior below 0

# contrasts on the observed (manifest) scale (p = probability correct)
m2_p = plogis(m2_logit)

plot_contr(x = m2_p[,1] - m2_p[,2], # remember which columns are which...
           title = "Condition A - B: Group 1", 
           x_lab = "A - B (p)")

plot_contr(x = m2_p[,3] - m2_p[,4], 
           title = "Condition A - B: Group 2", 
           x_lab = "A - B (p)")

# is the difference larger in group 1 vs group 2?
plot_contr(x = (m2_p[,1] - m2_p[,2]) - (m2_p[,3] - m2_p[,4]), 
           title = "Condition A - B: Group 1 vs 2", 
           x_lab = "A - B (p)")

# overall age difference 
p_group_diff = plogis((m2_p[,1] + m2_p[,2])/2) - plogis((m2_p[,3] + m2_p[,4])/2)
plot_contr(x = p_group_diff, 
           title = "Group 1 vs 2", 
           x_lab = "p difference")

mean(p_group_diff < 0) # proportion of the posterior below 0


## Savage-Dickey test ----
# for group:condition interaction (requires sample_prior = "yes")
(hy = hypothesis(m2, "group1:condition1 = 0"))

hy_p=plot(hy)[[1]]
# zoom in
hy_p + ggplot2::coord_cartesian(xlim = c(-.2, .2))

# Q 4
# using model 2, test the hypothesis
# that the effect of group is smaller 
# than zero


# uncomment line below to save objects
# save.image("examples/brms-example2.RData")








##### ANSWERS ------

# Q 1
# how to find priors for a particular model specification
get_prior(acc ~ group*condition + (1 + condition | id), data = dat, 
          family = bernoulli(link = "logit"))






# Q 2
# the model formula should be
acc ~ group*condition + (1 + condition | id) + (1 | item)







# Q 3
# add information needed to compare models
m1 = add_criterion(m1, c("loo", "waic", "marglik"))
m2 = add_criterion(m2, c("loo", "waic", "marglik"))







# Q 4
# test directional effect of group
(hy2 = hypothesis(m2, "group1 < 0"))

plot(hy2)


