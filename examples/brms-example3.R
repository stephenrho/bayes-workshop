### ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
### brms - example 3
### This example uses the 'wine' data set from the ordinal package
### Example adapted from:
### https://cran.r-project.org/web/packages/ordinal/vignettes/clmm2_tutorial.pdf
###
### There are questions for you to complete through the
### script. Answers are given at the end.
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

# average ratings
wine_means = aggregate(rating ~ contact + temp, data = wine, FUN = mean)

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

# Q 1 ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
# set the priors for theIntercept, b, and sd parameters
# you can use whatever prior you think are reasonable
# I used Intercept ~ normal(0, 3), b ~ normal(0, 1)
# sd ~ cauchy(0, 2)




## fit cumulative model  

# Q 2 ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
# In model 1 we want to predict rating by temp and contact
# while accounting for the nested structure in the data
# (ratings given by the same people)
# Complete the code below by specifying the model formula

m1 = brm( x , # <-----
         data=wine, family = cumulative(link = "probit"), 
         prior = priors, save_all_pars=T)

# this might throw a divergent transitions error, if so add
# control = list(adapt_delta = .9))
# to the brm() call

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
         prior = priors,
         save_all_pars=T, control = list(adapt_delta = .99))

summary(m2)

pp_check(m2, nsamples = 100)

marginal_effects(m2)

marginal_effects(m2, categorical = T)


## bayes factor
# does relaxing the variance=1 assumption improve model fit? 

m1 = add_criterion(m1, c("marglik"))
m2 = add_criterion(m2, c("marglik"))

bayes_factor(m1, m2)
# equal variances model (m1) does better than unequal (m2)


## extract posterior samples for specific constrasts
new_data = data.frame(temp=c("cold", "cold", "warm", "warm"),
                      contact=c("no", "yes", "no", "yes"))

m1_post = posterior_linpred(m1, newdata = new_data, transform = T, re_formula = NA)

# this gives us an array with probabilities of each rating
# [samples, new_data, rating]
dim(m1_post)

# here are the first 10 samples for rating 3
m1_post[1:10, , 3]

# to convert this into ratings were going to create a new matrix
m1_ratings = matrix(0, nrow = dim(m1_post)[1], ncol = dim(m1_post)[2])

# and fill it with the sum of the probabilities
# weighted by their rating (e.g., .1*1 + .2*2 + .5*3 + .05*4 + .15*5 = 2.95)
for (rating in 1:5){
  m1_ratings = m1_ratings + m1_post[,, rating]*rating
}

# the columns are the different temp/contact conditions
# so we can apply some functions across the columns
apply(m1_ratings, 2, mean)
apply(m1_ratings, 2, median)
apply(m1_ratings, 2, quantile, probs=c(.025, .975)) # credible interval
apply(m1_ratings, 2, HDInterval::hdi) # highest density interval

# compare these quantities to those from marginal effects
me1 = marginal_effects(m1)
me1$`temp:contact`

# Q 3 ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
# use the posterior rating samples to directly
# contrast conditions. (1) Constrast contact-yes 
# vs. contact-no when temp==cold, (2) contrasts
# contact-yes vs. contact-no when temp==warm, (3)
# contrast (1) and (2) to find difference in 
# contact effect between cold and warm



# Q 4 ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
# fit model 1 again but instead of using the 
# cumulative family, use the gaussian family
# look at posterior predictive checks to
# see the problems of analyzing ordinal data 
# as metric




# uncomment line below to save objects
# save.image("examples/brms-example3.RData")







##### ANSWERS ------

# Q 1
# priors for model 1
priors = set_prior("normal(0, 3)", class = "Intercept") +
  set_prior("normal(0, 1)", class = "b") +
  set_prior("cauchy(0, 2)", class = "sd")








# Q 2
# formula for model 1
rating ~ temp*contact + (1 | judge)








# Q 3
# Recall that the rows in new_data
# become the columns in the sample matrix
new_data
# the next line is not necessary but makes this clear
colnames(m1_ratings) <- with(new_data, paste(temp, contact, sep = "_"))
head(m1_ratings)

# contrast (1)
c1 = m1_ratings[,2] - m1_ratings[,1] # gives a vector of differences (for each step in the chain)
mean(c1)
HDInterval::hdi(c1) # or quantile()
hist(c1)

# contrast (2)
c2 = m1_ratings[,4] - m1_ratings[,3]
mean(c2)
HDInterval::hdi(c2)
hist(c2)

# contrast (3)
c3 = c2 - c1 # contrast the contrasts! 
mean(c3)
HDInterval::hdi(c3)
hist(c3)








# Q 4
# fit a normal model to ratings
m1_norm = brm(rating ~ temp*contact + (1 | judge),
          data=wine, family = gaussian(), 
          prior = priors, save_all_pars=T)

pp_check(m1_norm, nsamples=100)
# normal models isn't terrible 
# but misses the bumps at 2 & 5
# puts predictive density
# beyond 1 and 5. Compare to...
pp_check(m1, nsamples=100)

