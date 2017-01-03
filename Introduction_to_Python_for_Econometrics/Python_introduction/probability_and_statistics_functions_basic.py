from __future__ import division
from __future__ import print_function
import scipy
import scipy.stats as stats
import sys
from pylab import *
from numpy import *
# End Imports


x = rand(3,4,5)
y = random_sample((3,4,5))

x = randn(3,4,5)
y = standard_normal((3,4,5))

x = randint(0,10,(100))
x.max() # Is 9 since range is [0,10)
y = random_integers(0,10,(100))
y.max() # Is 10 since range is [0,10]

x = arange(10)
shuffle(x)
x

x = arange(10)
permutation(x)
x

gen1 = np.random.RandomState()
gen2 = np.random.RandomState()
gen1.uniform() # Generate a uniform
state1 = gen1.get_state()
gen1.uniform()
gen2.uniform() # Different, since gen2 has different seed
gen2.set_state(state1)
gen2.uniform() # Same uniform as gen1 produces after assigning state

st = get_state()
randn(4)
set_state(st)
randn(4)

seed()
randn()
seed()
randn()
seed(0)
randn()
seed(0)
randn()

x = arange(10.0)
x.mean()
mean(x)
x= reshape(arange(20.0),(4,5))
mean(x,0)
x.mean(1)

x= randn(4,5)
x
median(x)
median(x, 0)

x= randn(3,4)
corrcoef(x)
corrcoef(x[0],x[1])
corrcoef(x, rowvar=False)
corrcoef(x.T)

x = randn(1000)
count, binends = histogram(x)
count
binends
count, binends = histogram(x, 25)


gamma = stats.gamma
gamma.mean(2), gamma.median(2), gamma.std(2), gamma.var(2)
gamma.moment(2,2) - gamma.moment(1,2)**2 # Variance
gamma.cdf(5, 2), gamma.pdf(5, 2)
gamma.ppf(.95957232, 2)
log(gamma.pdf(5, 2)) - gamma.logpdf(5, 2)
gamma.rvs(5, size=(2,2))
gamma.fit(gamma.rvs(5, size=(1000)), floc = 0) # a, 0, shape

g = scipy.stats.gamma(1, scale=2)
g.cdf(1)

x=randint(1,11,1000)
stats.mode(x)

x = randn(1000)
moment = stats.moment
moment(x,2) - moment(x,1)**2
var(x)
x = randn(1000,2)
moment(x,2,0) # axis 0

x = randn(1000)
skew = stats.skew
skew(x)
x = randn(1000,2)
skew(x,0)

x = randn(1000)
kurtosis = stats.kurtosis
kurtosis(x)
kurtosis(x, fisher=False)
kurtosis(x, fisher=False) - kurtosis(x) # Must be 3
x = randn(1000,2)
kurtosis(x,0)

x = randn(10)
y = x + randn(10)
pearsonr = stats.pearsonr
corr, pval = pearsonr(x, y)
corr
pval

x = randn(10,3)
spearmanr = stats.spearmanr
rho, pval = spearmanr(x)
rho
pval
rho, pval = spearmanr(x[:,1],x[:,2])
corr
pval

x = randn(10)
y = x + randn(10)
kendalltau = stats.kendalltau
tau, pval = kendalltau(x,y)
tau
pval

x = randn(10)
y = x + randn(10)
linregress = stats.linregress
slope, intercept, rvalue, pvalue, stderr = linregress(x,y)
slope
rsquare = rvalue**2
rsquare
x.shape = 10,1
y.shape = 10,1
z = hstack((x,y))
linregress(z) # Alternative form, [x y]

x = randn(100)
kstest = stats.kstest
stat, pval = kstest(x, 'norm')
stat
pval
ncdf = stats.norm().cdf # No () on cdf to get the function
kstest(x, ncdf)
x = gamma.rvs(2, size = 100)
kstest(x, 'gamma', (2,)) # (2,) contains the shape parameter
gcdf = gamma(2).cdf
kstest(x, gcdf)

