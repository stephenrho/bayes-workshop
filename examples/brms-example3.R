### ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
### brms - example 3
### This example uses the 'wine' data set from the ordinal package
### Example adapted from:
### https://cran.r-project.org/web/packages/ordinal/vignettes/clmm2_tutorial.pdf
###
### This analysis demonstrates an appropriate analysis of 
### likert type data. The second model also begins to introduce more
### advanced formulas in brms (see vignette("brms_nonlinear", package="brms") & vignette("brms_distreg", package="brms") for more)
### 
### For more on ordinal models using brms, see:
### https://journals.sagepub.com/doi/abs/10.1177/2515245918823199
### ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

library(brms)

# speed things up
options(mc.cores=parallel::detectCores())

# data(wine, package = "ordinal")

wine = read.csv("examples/wine.csv")

wine = within(wine, {
  bottle = as.factor(bottle)
  judge = as.factor(judge)
})

with(wine, table(temp, contact, bottle))

# set contrasts
options(contrasts=c("contr.sum", "contr.sum"))
#options(contrasts=c("contr.treatment", "contr.treatment"))

contrasts(wine$temp)
contrasts(wine$contact)

## model 1 - cumulative model
# this model assumes that ratings arise via a latent underlying distribution
# which is divided by (in this case) 4 thresholds
# these thresholds are estimated along with shifts in the mean of 
# the distribution according to temp and contact (and judge)
# this model assumes the variance of the latent normal distribution
# is 1 and the same for all conditions. Model 2 relaxes this assumption

# set prior
get_prior(rating ~ temp*contact + (1 | judge), data = wine, 
          family = cumulative(link = "probit"))

priors = set_prior("normal(0, 3)", class = "Intercept") +
  set_prior("normal(0, 1)", class = "b") +
  set_prior("cauchy(0, 2)", class = "sd")

## fit cumulative model  
m1 = brm(rating ~ temp*contact + (1 | judge), data=wine,
         family = cumulative(link = "probit"), save_all_pars=T)

# this might throw a divergent transitions error, if so run below
# m1 = brm(rating ~ temp*contact + (1 | judge), data=wine,
#          family = cumulative(link = "probit"), save_all_pars=T,
#          control = list(adapt_delta = .9))

summary(m1)

pp_check(m1, nsamples = 100)

marginal_effects(m1, categorical = T) # plots probability of each rating
marginal_effects(m1, categorical = T, # to visualize two-way interaction
                 conditions = data.frame(temp=c("cold", "warm")), 
                 effects = "contact")

marginal_effects(m1) # plots mean predicted rating (without random effects)


## model 2 - unequal variances
# see pages 11-12 https://journals.sagepub.com/doi/abs/10.1177/2515245918823199
# one possible extension of the first model is to allow the variance of 
# the latent variable underlying ratings to vary as a function of conditions
# so in the code below, we extent the main model formula (bf) with an additional
# linear formula (lf) modeling the variance (disc) as a function of temp and contact
# see the paper for an explanation of why the intercept is excluded and cmc is set to F

m2 = brm(bf(rating ~ temp*contact + (1 | judge)) + 
           lf(disc ~ 0 + temp*contact, cmc = F), 
         data=wine, family = cumulative(link = "probit"),
         save_all_pars=T, control = list(adapt_delta = .99))

summary(m2)

pp_check(m2, nsamples = 100)

marginal_effects(m2, categorical = T) 


## bayes factor
# does relaxing the variance=1 assumption improve model fit? 

m1 = add_criterion(m1, c("marglik"))
m2 = add_criterion(m2, c("marglik"))

bayes_factor(m1, m2)
# equal variances model (m1) does better than unequal (m2)

 

