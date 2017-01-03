x = c(-0.86,-0.30, -0.05, 0.73)
n = rep(5,4)
y = c(0,1,3,5)

logit = function(p) log(p/(1-p))
invlogit = function(pred) exp(pred) / (1+exp(pred))

# Including the binomal coefficients - not necessary, but the easiest
L = function(alpha,beta) {
   a = prod(dbinom(y,n,invlogit(alpha+beta*x)))
   a
}

M = 80
a = seq(-5,10, length=M)
b = seq(-10,40,length=M)

Lmat = matrix(0,M,M)

for (i in 1:M) for (j in 1:M) Lmat[i,j] = L(a[i],b[j])
contour(a,b,Lmat)
abline(v=0.8466); abline(h=7.7488)

# Sampling from posterior

# Initialize
asim = NULL
bsim = NULL
aprob = NULL

# Marginal dist'n of alpha
print (dim(Lmat))
for (i in 1:M) aprob[i] = sum(Lmat[i,]) 

# Simulation on grid

for (r in 1:1000)
{
   aindex = sample(1:M, 1, prob=aprob)  ## sample alpha-index
   print (aprob)
   asim[r] = a[aindex]                 ## the corr. alpha-value   
   print (asim[r])
   quit()
   bprob = Lmat[aindex,]               ## the conditional dist'n
   bsim[r] = sample(b, 1, prob=bprob)  ## sample beta
}


# Jitter to get cont. dist'n (not on grid)

apost = asim + runif(1000, -0.075/2, 0.075/2)
bpost = bsim + runif(1000,-0.25/2, 0.25/2)

# Posterior inference:

plot(apost, bpost, xlim=c(-5,10), ylim=c(-10,40), pch='.')

