import numpy 
from matplotlib import pyplot as plt
from matplotlib import rc
import scikits.statsmodels.api as sm
from scipy import stats
import sim

data = numpy.loadtxt("../doc/gelman/ARM_Data/arsenic/wells.dat",  
                     usecols = (1,2,3,4,5), 
                     skiprows=1)

exog = data[:,2]
endog = data[:,0]

exog = sm.add_constant(exog, prepend=True)

logit_mod = sm.Logit(endog, exog)
logit_res = logit_mod.fit()

[beta, sigma] = sim.sim_glm(logit_res, 1000)

print numpy.mean(beta[:,0])
print numpy.mean(beta[:,1])

plt.plot(beta[:,0], beta[:,1], '.')
plt.xlabel('beta_0')
plt.ylabel('beta_1')
plt.show()
