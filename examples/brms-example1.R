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
### ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

library(brms)

# data("sleepstudy", package = "lme4")
# or
sleepstudy <- read.csv("examples/sleepstudy.csv")

sleepstudy$Subject = as.factor(sleepstudy$Subject)

summary(sleepstudy)

# plot data
plot(NA, xlim=c(0,9), ylim=c(190,500), axes=F, xlab="Days", ylab="Reaction", main="The sleepstudy dataset")
axis(1); axis(2)
lapply(unique(sleepstudy$Subject), 
       function(x) with(subset(sleepstudy, Subject==x), lines(Days, Reaction, col='grey')))
with(aggregate(Reaction~Days, FUN = mean, data = sleepstudy), points(Days, Reaction, pch=16, type='b', col='red'))
legend(0, 500, legend = c("Individual", "Average"), col = c("grey", "red"), pch = c(NA, 16), lty=c(1,1))

## fit the models ----
# Model 1: no random effects

# if you're not sure what priors are needed for your model, you can use 'get_prior()'
get_prior(Reaction ~ Days, data = sleepstudy, family = gaussian)

priors = c(set_prior("normal(300, 100)", class = "Intercept"),
           set_prior("normal(0, 50)", class = "b"),
           set_prior("cauchy(0, 50)", class = "sigma"))

m1 = brm(Reaction ~ Days, data = sleepstudy, 
         family = gaussian, 
         prior=priors,
         save_all_pars=T)

pp_check(m1, nsamples = 100)

# Model 2: allow intercept to vary by Subject
priors = c(priors,
           set_prior("cauchy(0, 100)", class = "sd"))

m2 = brm(Reaction ~ Days + (1 | Subject), data = sleepstudy, 
         family = gaussian, 
         prior=priors,
         save_all_pars=T)

pp_check(m2, nsamples=100)

# Model 3: allow intercept and effect of Days vary by participant

# get_prior(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy, family = gaussian)
priors = c(priors,
           set_prior("lkj(1)", class = "cor"))

m3 = brm(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy, 
         family = gaussian, 
         prior=priors,
         save_all_pars=T, 
         sample_prior = T) # for computing savage-dickey bayes factor below

pp_check(m3, nsamples=100)

plot(m3)

### more focused postrior predictive checks
y = sleepstudy$Reaction
yrep = posterior_predict(m3)


ppc_stat(y, yrep, stat = "mean")
ppc_stat(y, yrep, stat = "sd")
ppc_stat(y, yrep, stat = "max")

ppc_stat_grouped(y, yrep, stat = "mean", group = sleepstudy$Subject)

ppc_stat_grouped(y, posterior_predict(m1), stat = "mean", group = sleepstudy$Subject)


## compare the models ----

m1 = add_criterion(m1, c("loo", "waic", "marglik"))
m2 = add_criterion(m2, c("loo", "waic", "marglik"))
m3 = add_criterion(m3, c("loo", "waic", "marglik"))

plot(m3$loo)

loo_compare(m1, m2, m3, criterion = "loo")
loo_compare(m1, m2, m3, criterion = "waic")


bayes_factor(m2,m1)
bayes_factor(m3,m2)

post_prob(m1,m2,m3)

## savage-dickey bayes factor for effects of Days ----

(m3_sd = hypothesis(x = m3, hypothesis = "Days = 0"))

plot(m3_sd)

## do more with posterior samples ---- 
fixef(m3)
ranef(m3)

# extract the posterior samples for the effect of Days
m3_days_post = posterior_samples(m3, pars = "b_Days")[,1]

hist(m3_days_post, breaks = 30, border = F, col="lightblue", main="", xlab="", ylab="", axes=F)
axis(1)
lines(x = quantile(m3_days_post, probs = c(.025, .975)), y=c(0,0), lwd=3)
text(10, 50, labels = sprintf("%.2f [%.2f, %.2f]", mean(m3_days_post), 
                              quantile(m3_days_post, probs = c(.025)), 
                              quantile(m3_days_post, probs = c(.975))))

marginal_effects(m3)

# make predictions for a new person
newdat = data.frame(Days = 0:9, Subject=100)

m3_newp_pp = posterior_linpred(m3, newdata = newdat, allow_new_levels=T)

apply(m3_newp_pp, 2, FUN = function(x) c(mean=mean(x), quantile(x, probs = c(.025,.975))))

# plot 1000 posterior predictions
plot(NA, xlim=c(1,10), ylim=c(100,500), axes=F, xlab="Days", ylab="Reaction", main="posterior predictions for new person")
axis(1, at=1:10, labels = 0:9); axis(2)
lapply(sample(4000, size = 1000), 
       function(x) lines(m3_newp_pp[x,], col=rgb(.2,.2,.2,.2)))

# make predictions for mean
newdat = data.frame(Days = 0:9)

m3_mean_pp = posterior_linpred(m3, newdata = newdat, re_formula = NA)

apply(m3_mean_pp, 2, FUN = function(x) c(mean=mean(x), quantile(x, probs = c(.025,.975))))

plot(NA, xlim=c(1,10), ylim=c(200,400), axes=F, xlab="Days", ylab="Reaction", main="posterior predictions for average")
axis(1, at=1:10, labels = 0:9); axis(2)
lapply(sample(4000, size = 1000), 
       function(x) lines(m3_mean_pp[x,], col=rgb(.2,.2,.2,.2)))

# to add the observed means, uncomment the line below
# with(aggregate(Reaction~Days, FUN = mean, data = sleepstudy), points(Days+1, Reaction, pch=16, type='b', col='red'))

# alternative plot... posterior mean and 95% credible interval

x = t(apply(m3_mean_pp, 2, FUN = function(x) c(mean=mean(x), quantile(x, probs = c(.025,.975)))))
x=data.frame(x)
colnames(x) = c("mean", "lower", "upper")

plot(NA, xlim=c(1,10), ylim=c(200,400), axes=F, xlab="Days", ylab="Reaction", main="posterior predictions for average")
axis(1, at=1:10, labels = 0:9); axis(2)
with(x, polygon(x = c(1:10, 10:1), y = c(lower, rev(upper)), col = "lightblue", border = NA))
with(x, lines(1:10, mean))

## Possible extensions to the model ---- 

# mainly to demonstrate the additional functionality of brms...

# monotonic predictor

get_prior(Reaction ~ mo(Days) + (1 + mo(Days) | Subject), data = sleepstudy, family = gaussian)

m4 = brm(Reaction ~ mo(Days) + (1 + mo(Days) | Subject), data = sleepstudy, 
         family = gaussian, 
         prior=priors,
         save_all_pars=T, 
         sample_prior = T) 

m4 = add_criterion(m4, c("loo", "waic", "marglik"))

loo_compare(m3, m4, criterion = "loo")

bayes_factor(m3,m4)

# distributional model (allow residual SD to vary with days?)

m5_form = bf(Reaction ~ Days + (1 + Days | Subject), sigma ~ Days)

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


