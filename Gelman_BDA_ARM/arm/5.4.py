import numpy 
from matplotlib import pyplot as plt
import statsmodels.api as sm
from scipy import stats

data = numpy.loadtxt("../doc/gelman/ARM_Data/arsenic/wells.dat",  
                     usecols = (1,2,3,4,5), 
                     skiprows=1)

exog = data[:,2]
endog = data[:,0]

exog = sm.add_constant(exog)

logit_mod = sm.Logit(endog, exog)
logit_res = logit_mod.fit()

print logit_res.params
print logit_res.bse
print logit_res.df_resid
