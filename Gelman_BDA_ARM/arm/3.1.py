############# Using leastsq ########################

import numpy
from scipy.optimize import leastsq  

ks = numpy.loadtxt("kidscore.dat",  skiprows=1)

def residuals(parameters,y,x):
    a = parameters[0]
    b = parameters[1]
    err = abs(y - (a*x + b))
    return err

p0 = [11,80]

fit = leastsq(residuals,p0,args=(ks[:,1], ks[:,2]))

print fit

############ Using Scikits Statsmodels #####################

import numpy
from matplotlib import pyplot as plt
import scikits.statsmodels.api as sm

ks = numpy.loadtxt("kidscore.dat",  skiprows=1)

olsmod = sm.OLS(ks[:,1], sm.add_constant(ks[:,2]))
olsres = olsmod.fit()
print olsres.summary()

