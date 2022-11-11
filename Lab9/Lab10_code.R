
set.seed(341)
library(extraDistr)

#### Activity 1 ####
p1 = rep(1/6, 10)

# Q2
dmultinom(x=rep(10,10), prob=p1)

# Q3
dmultinom(x=c(8, 12, 9, 10, 10, 8, 15, 13, 7, 8), prob=p1)

# Q4
Ys = rmultinom(n=20, size=100, prob=p1)
rowMeans(Ys)
apply(Ys, 1, var)
cov(t(Ys))


#### Activity 2 ####
p2 = c(9, 3, 3, 1)/16
  
# Q6
dmultinom(x=c(225, 75, 75, 25), prob=p2)

# Q7
dmultinom(x=c(247, 53, 85, 15), prob=p2)


#### Activity 3 ####

# Q8
a1 = rep(1,10)
ps_equal = rdirichlet(n=100, alpha=a1)
x1 = rowSums(apply(ps_equal, 1, function(x){ rmultinom(n=1, size=1, prob=x) }))
x1

# Q9
dmultinom(x=x1, prob=p1)

# Q10
a2 = seq(1,10,1)
ps_biased = rdirichlet(n=100, alpha=a2)
x2 = rowSums(apply(ps_biased, 1, function(x){ rmultinom(n=1, size=1, prob=x) }))
x2

# Q11
dmultinom(x=x2, prob=p1)

