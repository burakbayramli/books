import numpy
from matplotlib import pyplot as plt
import statsmodels.api as sm
import sim

ks = numpy.loadtxt("kidscore.dat",  skiprows=1)
olsmod = sm.OLS(ks[:,1], sm.add_constant(ks[:,3], prepend=True))
olsres = olsmod.fit()
print olsres.summary()
plt.plot (ks[:,3], ks[:,1], '.')

# simulate data points
beta, sigma = sim.sim_lm(olsres)

# plot simulation

x = numpy.linspace(60,150,100)

for i in range(10): 
    plt.plot(x, beta[i,0] + beta[i,1]*x, 'y')

plt.plot(x, olsres.params[0] + olsres.params[1]*x, 'b')
plt.xlabel("Mother IQ Score")
plt.ylabel("Child Test Score")
        
plt.show()
