### ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
### Simulate 2D-recall data for stan example 2
### ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

sample_locs = function(rad, inner=0, n=1000){
  a = inner/rad
  r = runif(n, a^2, 1)
  theta = runif(n, 0, 2*pi)
  x = sqrt(r)*cos(theta)*rad
  y = sqrt(r)*sin(theta)*rad
  
  return(cbind(x,y))
}

sim_mix = function(m, s, n=1000){
  p = sample_locs(10, n=n)
  # sample number of responses from memory
  mem = rbinom(n = n, size = 1, prob = m)
  # sample x and y locations for memory responses
  x = rnorm(n = sum(mem), mean = p[1:sum(mem),1], sd = s)
  y = rnorm(n = sum(mem), mean = p[1:sum(mem),2], sd = s)
  # sample guesses
  g = sample_locs(10, n = n - sum(mem))
  # put in one matrix
  out = rbind(cbind(x,y), g)
  out = cbind(p, out)
  colnames(out) = c("px", "py", "rx", "ry")
  
  return(round(out, 3))
}


plot(sample_locs(10))


I = 10 # n participants
J = 100 # n trials

m_pars = c(1, 2) # mean and sd on logit scale
s_pars = c(.1, .5) # mean and sd on log scale

set.seed(123)
m_i = rnorm(I, m_pars[1], m_pars[2])
s_i = rnorm(I, s_pars[1], s_pars[2])

hist(plogis(m_i))
hist(exp(s_i))

N = I*J

recall_2d = expand.grid(trial=1:J, id=1:I, px=NA, py=NA, rx=NA, ry=NA)

for (i in 1:I){
  recall_2d[recall_2d$id==i,3:6] = sim_mix(m = plogis(m_i[i]), s = exp(s_i[i]), n = J)
}

pairs(~ px+py+rx+ry, data = recall_2d)

with(recall_2d, hist(sqrt((px-rx)^2 + (py-ry)^2), breaks=30, 
                     col="grey", border=F, main="Recall Error", 
                     xlab="", probability = T))
with(recall_2d, lines(density(sqrt((px-rx)^2 + (py-ry)^2), from=0)))
                     
write.csv(x = recall_2d, file = "examples/recall-2D.csv", row.names = F)



