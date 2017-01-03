import numpy
import scipy.stats
import matplotlib.pyplot as plt
import random

x = numpy.array([-0.86,-0.30,-0.05,0.73])
n = numpy.array([5,5,5,5])
y = numpy.array([0,1,3,5])

def logit(p): 
    return numpy.log(p/(1-p))

def invlogit(pred): 
    return numpy.exp(pred) / (1+numpy.exp(pred))

def L(alpha,beta): 
    prod = numpy.product(scipy.stats.binom.pmf(y,n,invlogit(alpha+beta*x)))
    return prod

def w_choice_unnormalized(lst):
    lst = lst / sum(lst) # normalize
    n = random.uniform(0, 1)
    for item, weight in enumerate(lst):
        if n < weight:
            break
        n = n - weight
    return item

###############################
### calculate a,b

M = 80

a = numpy.linspace(-5.,10.,num=M)
b = numpy.linspace(-10.,40.,num=M)

Lmat = numpy.zeros((M, M))
for i in range(M):
    for j in range(M):
        Lmat[i,j] = L(a[i],b[j])
        
plt.contour(a,b,Lmat)
plt.show()

###############################
### Sample from the posterior

K = 1000
        
aprob = numpy.zeros((M))
asim = numpy.zeros(K)
bsim = numpy.zeros(K)

for i in numpy.arange(M): 
    aprob[i] = numpy.sum(Lmat[i,]) 
    
aprob[numpy.isnan(aprob)] = 0.0001

for r in numpy.arange(K):
   aindex = w_choice_unnormalized(aprob)      
   asim[r] = a[aindex]               ## the corr. alpha-value
   bprob = Lmat[aindex,]             ## the conditional dist'n
   bsim[r] = b[w_choice_unnormalized(bprob)]      ## sample beta

   
apost = asim + scipy.stats.uniform.rvs(loc=-0.075/2, scale=0.075, size=K)
bpost = bsim + scipy.stats.uniform.rvs(loc=-0.075/2, scale=0.075, size=K)

plt.plot(apost, bpost, '.')
plt.xlim(-5,10)
plt.ylim(-10,40)
plt.show()
