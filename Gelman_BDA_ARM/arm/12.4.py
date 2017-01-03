import rpy2.robjects as robjects
import rpy2.rinterface as rinterface
from rpy2.robjects.packages import importr
import rpy2.robjects.numpy2ri
import numpy as np

lme4 = importr("lme4")
srss2 = np.recfromcsv('../doc/gelman/ARM_Data/radon/srrs2.dat')
activity = srss2['activity']
state = srss2['state']
floor = srss2['floor']
county = srss2['county']

radon = activity[state == 'MN']
radon[radon == 0] = 0.1

floor = floor[state == 'MN']

log_radon = np.log(radon)

n = len(radon)
y = log_radon
x = floor

county_name = county[state == 'MN']
county = county[state == 'MN']
uniq = np.unique(county_name)
J = len(uniq)

cc = np.nan * np.ones(len(county))
for i in np.arange(J):
    cc[county_name==uniq[i]] = i+1

robjects.r.assign('y',y)
robjects.r.assign('x',x)
robjects.r.assign('county',county)

res = robjects.r.lmer('y ~ 1 + (1 | county)')
res = robjects.r.coef(res)
for x in res.iteritems(): 
    for i in range(len(x[1][0])):
        print "val", x[1][0][i]
