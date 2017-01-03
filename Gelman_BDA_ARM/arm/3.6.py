import numpy
from matplotlib import pyplot as plt
import scikits.statsmodels.api as sm
import sim

ks = numpy.loadtxt("kidscore.dat",  skiprows=1)
olsmod = sm.OLS(ks[:,1], sm.add_constant(ks[:,3], prepend=True))
olsres = olsmod.fit()

sd = numpy.std(olsres.resid)

plt.plot(ks[:,3], olsres.resid,'.')

plt.plot([70,140],[sd,sd], 'b--')
plt.plot([70,140],[-sd,-sd], 'b--')
plt.ylabel("Residuals")
plt.xlabel("Mother IQ Score")

plt.show()


