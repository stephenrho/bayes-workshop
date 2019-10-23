### ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
### brms - example 1
### In this example we use the 'sleepstudy' data set
### which contains the average reaction times of 
### participants with 0-9 days of sleep deprivation
### 
### We start with a simple regression model that 
### does not account for the clustering in the data
### (mutliple RTs from the same Subjects) and then 
### build on this model to include random effects
###
### This example introduces gaussian models in brms,
### comparing models (loo, waic, bayes_factor),
### posterior predictive checks, working with posterior samples
###
### This example is used throughout the brms.html slides
### ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

library(brms)
library(bayesplot)

# data("sleepstudy", package = "lme4")
# or
sleepstudy <- read.csv("examples/sleepstudy.csv")

sleepstudy$Subject = as.factor(sleepstudy$Subject)

summary(sleepstudy)

# plot data
plot(NA, xlim=c(0,9), ylim=c(190,500), axes=F, xlab="Days", ylab="Reaction", main="The sleepstudy dataset")
axis(1); axis(2)
lapply(unique(sleepstudy$Subject), # ignore the NULLs...
       function(x) with(subset(sleepstudy, Subject==x), lines(Days, Reaction, col='grey')))
with(aggregate(Reaction~Days, FUN = mean, data = sleepstudy), points(Days, Reaction, pch=16, type='b', col='red'))
legend(0, 500, legend = c("Individual", "Average"), col = c("grey", "red"), pch = c(NA, 16), lty=c(1,1))

## Model 1: no random effects ----

# if you're not sure what priors are needed for your model, you can use 'get_prior()'
get_prior(Reaction ~ Days, data = sleepstudy, family = gaussian)

priors = c(set_prior("normal(500, 100)", class = "Intercept"),
           set_prior("normal(0, 50)", class = "b"),
           set_prior("cauchy(0, 50)", class = "sigma"))

# prior predictive samples 
p1 = brm(Reaction ~ Days, data = sleepstudy, 
         family = gaussian, 
         prior=priors,
         sample_prior = "only") # to sample only from the prior - "yes" or T = both prior and posterior, "no" or F = posterior only (default)

# plot the prior predictions
marginal_effects(p1, method="predict")
marginal_effects(p1, conditions = data.frame(Subject=unique(sleepstudy$Subject)), method="predict")
# not great, see negative reaction time predictions, but not bad

# fit model 1 (i.e., sample from the posterior)
m1 = brm(Reaction ~ Days, data = sleepstudy, 
         family = gaussian, 
         prior=priors,
         save_all_pars=T) # for bridgesampling (to get marglik) see below

summary(m1)

# compare observed distribution (y) to posterior predictive (yrep)
pp_check(m1, nsamples = 100)

# use the bayesplot package to do more focused PPCs

yrep = posterior_predict(m1)
ppc_stat(y, yrep, stat = "mean")
ppc_stat(y, yrep, stat = "sd")
ppc_stat(y, yrep, stat = "max")

# the model does bad here as the parameters are fixed across groups (i.e., Subject)
ppc_stat_grouped(y, yrep, stat = "mean", group = sleepstudy$Subject)

## Model 2: allow intercept to vary by Subject ----

# we need to add an extra prior for the random intercept (class="sd") - see ?set_prior
priors = c(priors,
           set_prior("cauchy(0, 100)", class = "sd"))

# if you're interested in the prior predictive samples, uncomment below
# p2 = brm(Reaction ~ Days + (1 | Subject), data = sleepstudy, 
#          family = gaussian, 
#          prior=priors,
#          sample_prior = "only")
# 
# marginal_effects(p2, method="predict")
# marginal_effects(p2, conditions = data.frame(Subject=unique(sleepstudy$Subject)), method="predict")

# sample from posterior
m2 = brm(Reaction ~ Days + (1 | Subject), data = sleepstudy, 
         family = gaussian, 
         prior=priors,
         save_all_pars=T)

summary(m2) # note the new "Group-Level Effects:" section 

pp_check(m2, nsamples=100)

yrep = posterior_predict(m2) # replace old yrep
ppc_stat_grouped(y, yrep, stat = "mean", group = sleepstudy$Subject)
# ppc_stat_grouped(y, yrep, stat = "min", group = sleepstudy$Subject)

# looks much better!

## Model 3: allow intercept and effect of Days vary by participant ----

# we need to add a new prior for the correlation between random slope and intercept
# get_prior(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy, family = gaussian)
priors = c(priors,
           set_prior("lkj(1)", class = "cor")) # uniform prior (see intro slides)

m3 = brm(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy, 
         family = gaussian, 
         prior=priors,
         save_all_pars=T, 
         sample_prior = T) # for computing savage-dickey bayes factor - see below

summary(m3) # more stuff in "Group-Level Effects:"

pp_check(m3, nsamples=100)

plot(m3, pars = "b_") # plot the chains for the fixed effects
# see parnames(m3) for all parameters

y = sleepstudy$Reaction
yrep = posterior_predict(m3)

ppc_stat(y, yrep, stat = "mean")
ppc_stat(y, yrep, stat = "sd")
ppc_stat(y, yrep, stat = "max")

ppc_stat_grouped(y, yrep, stat = "mean", group = sleepstudy$Subject)

# lets look at population level predictions
marginal_effects(m3)

# and predictions for each individual
plot(marginal_effects(m3, # exchange with other models to compare (points don't work for m1 as Subject wasn't in the model)
                      conditions = data.frame(Subject=unique(sleepstudy$Subject)), 
                      re_formula = NULL), points=T)


## Compare the models ----

m1 = add_criterion(m1, c("loo", "waic", "marglik"))
m2 = add_criterion(m2, c("loo", "waic", "marglik"))
m3 = add_criterion(m3, c("loo", "waic", "marglik"))

# model 3 has some problematic observations (that we will ignore for sake of time)
# but see https://www.rdocumentation.org/packages/loo/versions/2.1.0/topics/loo-glossary
# and https://rdrr.io/cran/loo/man/pareto-k-diagnostic.html
# one option would be to use add_criterion(m3, c("loo", "waic", "marglik"), reloo=T)
# which fits the model without problem observations (can take a while)

loo_compare(m1, m2, m3, criterion = "loo")
loo_compare(m1, m2, m3, criterion = "waic")

bayes_factor(m2,m1)
bayes_factor(m3,m2)

post_prob(m1, m2, m3, prior_prob = c(1/3, 1/3, 1/3)) # the evidence for m3 is so strong that prior_prob doesn't really matter...

## Savage-Dickey Bayes factor for effects of Days ----

m3_sd = hypothesis(x = m3, hypothesis = "Days = 0")

plot(m3_sd)
# this isn't a particularly great example as all of the posterior samples are > 0
# the other analyses scripts give some better examples...

## Do more with posterior samples ---- 
fixef(m3)
ranef(m3)

# extract the posterior samples for the effect of Days
m3_days_post = posterior_samples(m3, pars = "b_Days")[,1]

# plot the histogram and 95% CI
hist(m3_days_post, breaks = 30, border = F, col="lightblue", main="", xlab="", ylab="", axes=F)
axis(1)
lines(x = quantile(m3_days_post, probs = c(.025, .975)), y=c(0,0), lwd=3)
text(mean(m3_days_post), 50, labels = sprintf("%.2f [%.2f, %.2f]", mean(m3_days_post), 
                              quantile(m3_days_post, probs = c(.025)), 
                              quantile(m3_days_post, probs = c(.975))))

# make predictions for a new person
newdat = data.frame(Days = 0:9, Subject=100)
m3_newp_pp = posterior_linpred(m3, newdata = newdat, 
                               re_formula = NULL, # default, includes random effects
                               allow_new_levels=T) # allows predictions for new levels of Subject

apply(m3_newp_pp, 2, FUN = function(x) c(mean=mean(x), quantile(x, probs = c(.025,.975))))

# plot 1000 posterior predictions
plot(NA, xlim=c(1,10), ylim=c(100,500), axes=F, xlab="Days", ylab="Reaction", main="posterior predictions for new person")
axis(1, at=1:10, labels = 0:9); axis(2)
lapply(sample(4000, size = 1000), 
       function(x) lines(m3_newp_pp[x,], col=rgb(.2,.2,.2,.2)))

# we can get something similar from marginal effects by including random effects
marginal_effects(m3, re_formula = NULL)

# make predictions for mean (population)
newdat = data.frame(Days = 0:9)
m3_mean_pp = posterior_linpred(m3, newdata = newdat, re_formula = NA)

apply(m3_mean_pp, 2, FUN = function(x) c(mean=mean(x), quantile(x, probs = c(.025,.975))))

plot(NA, xlim=c(1,10), ylim=c(200,400), axes=F, xlab="Days", ylab="Reaction", main="posterior predictions for average")
axis(1, at=1:10, labels = 0:9); axis(2)
lapply(sample(4000, size = 1000), 
       function(x) lines(m3_mean_pp[x,], col=rgb(.2,.2,.2,.2)))

# to add the observed means, uncomment the line below
# with(aggregate(Reaction~Days, FUN = mean, data = sleepstudy), points(Days+1, Reaction, pch=16, type='b', col='red'))

# something similar from marginal_effects
marginal_effects(m3)



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  
#### EXTRA STUFF -----
## this goes beyond what we cover in the slides
## it covers possible extentions to the models
## considered above. This is mainly to show 
## additional functionality of brms...

### monotonic predictor
# it may be that the change in reaction time with days
# is monotonically increasing but not at a consistent 
# rate (i.e., the change across adjacent days may differ)
# it is possible to incorporate these assumptions and model
# days as a 'monotonic effect' using mo() 
# see https://psyarxiv.com/9qkhj/ for detail

get_prior(Reaction ~ mo(Days) + (1 + mo(Days) | Subject), data = sleepstudy, family = gaussian)

m4 = brm(Reaction ~ mo(Days) + (1 + mo(Days) | Subject), data = sleepstudy, 
         family = gaussian, 
         prior=priors,
         save_all_pars=T, 
         sample_prior = T) 

m4 = add_criterion(m4, c("loo", "waic", "marglik"))

loo_compare(m3, m4, criterion = "loo")

bayes_factor(m3,m4)

### distributional model
# with increasing days of sleep deprivation
# there may be increasing measurement error
# from sources not included in our model.
# brms allows you to model other parameters
# of the response distribution (i.e, not just the mean).
# So we can let the residual variance (sigma) vary.
# Below we just model sigma with a fixed effect of 
# Days but you could consider more complex models
# see vignette("brms_distreg", package = "brms")

m5_form = bf(Reaction ~ Days + (1 + Days | Subject), 
             sigma ~ Days) # model sigma as a function of days

get_prior(m5_form, data = sleepstudy, family = gaussian)

priors = c(set_prior("normal(300, 100)", class = "Intercept"),
           set_prior("normal(0, 50)", class = "b"),
           set_prior("cauchy(0, 50)", class = "b", dpar = "sigma"),
           set_prior("lkj(1)", class = "cor"))

m5 = brm(m5_form, data = sleepstudy, 
         family = gaussian, 
         prior=priors,
         save_all_pars=T, 
         sample_prior = T) 

m5 = add_criterion(m5, c("loo", "waic", "marglik"))

loo_compare(m3, m5, criterion = "loo")

bayes_factor(m3, m5)


# save to load objects in brms.Rmd
# save.image("examples/brms-example1.RData")


