import pymc
import radon_varying_intercept
from pylab import hist, show
from pymc import Matplot

M = pymc.MCMC(radon_varying_intercept)
M.sample(iter=3000, burn=1000, thin=5)

fit = M.stats()
print (fit)
for k in fit.keys():
     print(k,fit[k]['mean'])


Matplot.plot(M)
