### ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
### stan - example 2
### fitting a mixture model to responses in two-dimensions
### data are from a hypothetical experiment where participants
### recall a location. Responses are assumed to either be centered 
### on the correct location (from memory) with some imprecision
### or are random guesses on some area, A
### ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

recall_2d = read.csv("examples/recall-2D.csv")


data_list = list(
  N = nrow(recall_2d),
  S = length(unique(recall_2d$id)),
  id = recall_2d$id,
  p_x = recall_2d$px,
  p_y = recall_2d$py,
  r_x = recall_2d$rx,
  r_y = recall_2d$ry,
  A = pi*10^2
)

fit <- stan(
  file = "examples/stan-models/example2.stan",
  data = data_list,
  chains = 4,
  warmup = 1000,
  iter = 2000,
  cores = 4
)


plot(fit)
plot(fit, pars="m")
plot(fit, pars="s")

mu_m = extract(fit, pars="mu_m")[[1]]
mu_s = extract(fit, pars="mu_s")[[1]]

par(mfrow=c(1,2))
hist(plogis(mu_m), col="lightblue", border=F, xlab="", main=bquote(mu[m]), probability = T)
hist(exp(mu_s), breaks=30, col="violet", border=F, xlab="", main=bquote(mu[s]), probability = T)
par(mfrow=c(1,1))


# individual parameters transformed back to manifest scale
pmem = plogis(extract(fit, pars="m")[[1]])
sigma = exp(extract(fit, pars="s")[[1]])

S = length(unique(recall_2d$id))

par(mfrow=c(1,2))
# plot individual m
plot(NA, xlim=c(0,1), ylim=c(1, S), xlab="m", ylab="id", axes=F)
axis(1); axis(2, at = 1:S, las=1)
for (i in 1:S){
  segments(x0 = quantile(pmem[,i], .025), y0 = i, x1 = quantile(pmem[,i], .975), y1 = i)
  points(mean(pmem[,i]), i, pch=16, col="red")
}

mtext("Individual Paramaters (means and 95% CIs)", 3, adj=0, font=2, line = 1.3)

# individual s
plot(NA, xlim=c(0,4), ylim=c(1, S), xlab="s", ylab="id", axes=F)
axis(1); axis(2, at = 1:S, las=1)
for (i in 1:S){
  segments(x0 = quantile(sigma[,i], .025), y0 = i, x1 = quantile(sigma[,i], .975), y1 = i)
  points(mean(sigma[,i]), i, pch=16, col="red")
}
par(mfrow=c(1,1))



# posterior predictive...
sample_locs = function(rad=10, n=2){
  r = runif(n, 0, 1)
  theta = runif(n, 0, 2*pi)
  x = sqrt(r)*cos(theta)*rad
  y = sqrt(r)*sin(theta)*rad
  
  return(cbind(x,y))
}

pp_sim = function(m, s){
  mem = rbinom(1,1,m)
  if (mem){
    x = rnorm(2, 0, sd = s)
    err = sqrt(sum(x^2))
  } else {
    x = sample_locs()
    err = sqrt((x[1,1] - x[2,1])^2 + (x[1,2] - x[2,2])^2)
  }
  
  return(unname(err))
}

# look at posterior predictive distribution for recall error
recall_2d$error = with(recall_2d, sqrt((px - rx)^2 + (py - ry)^2))

yrep_error = matrix(NA, ncol = nrow(recall_2d), nrow = nrow(pmem))

for (r in 1:nrow(recall_2d)){
  id = recall_2d$id[r]
  yrep_error[,r] = unlist(lapply(1:nrow(pmem), 
                                function(x) pp_sim(m = pmem[x, id], s = sigma[x, id])))
}

pp_rows = sample(4000, 100)
ppc_dens_overlay(y = recall_2d$error, yrep = yrep_error[pp_rows, ])

ppc_stat(y = recall_2d$error, yrep = yrep_error[pp_rows, ])
ppc_stat(y = recall_2d$error, yrep = yrep_error[pp_rows, ], stat = 'max')
ppc_stat(y = recall_2d$error, yrep = yrep_error[pp_rows, ], stat = 'min')


