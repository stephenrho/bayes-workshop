### ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 
### Simulate data for brms example 2
### Example recall experiment with 26 items (e.g. words)
### two groups (S participants each) 
### and two repeated measures conditions (items recalled in both conditions)
### acc = recall accuracy
### there is between item variability and random participant 
### variability in both intercept and slope
### fixed effects of both group and condition but 
### no interaction (on logit scale)
### ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ 

S = 20

out = expand.grid(id = 1:(S*2), item = 1:26, condition=c(1,2))

N = nrow(out)

out$group = ifelse(out$id > S, 2, 1)

# 'fixed' effects
mu_int = qlogis(.7)
mu_cond = .4
mu_group = .2

# 'random' effects
sd_int = .5
sd_cond = .5
rho_int_cond = .3

sd_item = .3

d = diag(c(sd_int, sd_cond))
r = matrix(c(1,rho_int_cond, rho_int_cond,1), nrow = 2, ncol = 2)

cov_mat = d%*%r%*%d

# generate participant and item effects
b_s = MASS::mvrnorm(S*2, mu = c(0,0), Sigma = cov_mat)
b_i = rnorm(26, mean = 0, sd = sd_item)

# calculate expected logit(probability) of a correct response for each row
logit_p = with(out, mu_int + b_s[id,1] + (mu_cond + b_s[id,2])*c(-1,1)[condition] + mu_group*c(-1,1)[group] + b_i[item])

# simulate accuracy
out$acc = rbinom(n = N, size = 1, prob = plogis(logit_p))

# group/condition means (proportion correct)
out_means = aggregate(acc ~ condition + group, data = out, FUN = mean)

# item differences
plot(aggregate(acc ~ item, data = out, FUN = mean))
plot(b_i, aggregate(acc ~ item, data = out, FUN = mean)$acc) # by simulated deviation from mean

# aggregate proportion correct for each participant in each condition
out_agg = aggregate(acc ~ id + condition + group, data = out, FUN = mean)


## plot individuals and means...
cols_l = viridis::viridis(2, begin = .2, end = .8, alpha = .5)
cols_d = viridis::viridis(2, begin = .2, end = .8)

ids = list()
ids[[1]] = 1:20
ids[[2]] = 21:40

jitts = c(-.05, .05) # so groups are side-by-side

plot(NA, xlim=c(.7, 2.3), ylim=c(0,1), xlab="Condition", ylab="Accuracy", axes=F)
axis(1, at=1:2, labels = c("A", "B"))
axis(2)

for (g in c(1, 2)){
  # individual data points
  lapply(ids[[g]], function(x) with(subset(out_agg, id==x), points(jitter(condition+jitts[g], amount = .025), acc, pch=16, col=cols_l[g], type='p')))
}
for (g in c(1, 2)){
  with(subset(out_means, group==g), points(condition, acc, pch=21, bg=cols_d[g], type='b'))
}
legend(x = 1.5, y = .2, legend = c("1", "2"), pch = 21, pt.bg = cols_d, title = "Group", xjust = .5, yjust=.5, bty='n')

#### Make the data less 'numbery' and write 

out = as.data.frame(out)

out$item = LETTERS[out$item]
out$condition = c("A", "B")[out$condition]

out = out[order(out$id),]

write.csv(x = out, file = "examples/accuracy-data.csv", row.names = F)


