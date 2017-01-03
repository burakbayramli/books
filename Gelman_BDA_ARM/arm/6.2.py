# runs without error but cannot tell which result is the intercept
# which are the predicates. 

import numpy 
from matplotlib import pyplot as plt
import scikits.statsmodels.api as sm
from scipy import stats

data = numpy.loadtxt("../doc/gelman/arm2/police/frisk_with_noise.dat",  skiprows=7)

X = numpy.zeros((3,len(data[:,0])))
print X.shape

arrests = data[:,2]
arrests[arrests == 0] = 1
arrests = numpy.log(arrests)

stops = data[:,0]
stops[stops==0.0] = .0001

X[0,:] = arrests # arrests
X[1,:] = data[:,4] # eth
X[2,:] = numpy.ones(len(data[:,0])) # eth

glm = sm.GLM(stops, X.T, family=sm.families.Poisson())
res = glm.fit()

print "res.deviance=" + str(res.deviance)
print "res.scale=" + str(res.scale)
print "res.params=" + str(res.params)
print "res.pearson_chi2=" + str(res.pearson_chi2)
print "res.df_model=" + str(res.df_model)
print "res.null_deviance=" + str(res.null_deviance)
print "res.t()=" + str(res.t())


