from rpy2.robjects.vectors import FloatVector, StrVector
import rpy2.robjects as R
import rpy2.rinterface as rinterface
from rpy2.robjects.packages import importr
import rpy2.robjects.numpy2ri
import numpy as np
r2jags = importr('R2jags')

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

stfips = srss2['stfips']
cntyfips = srss2['cntyfips']
srss2_fips = stfips*1000 + cntyfips

cty = np.recfromcsv('cty.dat')
usa_fips = 1000*cty['stfips'] + cty['ctfips']
mn = srss2['state'] == 'MN'
match = np.in1d(usa_fips, np.unique(srss2_fips[mn]))
usa_rows = np.arange(0, len(usa_fips))[match]
cty_uranium = cty['uppm']
uranium = cty_uranium[match]
u = np.log(uranium)

county = cc

R.r.assign('n',n)
R.r.assign('J',J)
R.r.assign('y',y)
R.r.assign('county',county)
R.r.assign('x',x)
R.r.assign('u',u)

radon_data = StrVector(("n", "J", "y", "county", "x"))
radon_params = StrVector(("a", "b", "sigma.y", "sigma.a"))
radon_inits = R.r('''function (){
  list (a=rnorm(J), b=rnorm(1), g.0=rnorm(1), g.1=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
  }''')

radon_1 = r2jags.jags(data = radon_data, inits = radon_inits,
                      parameters_to_save = radon_params,
                      n_iter = 100,
                      n_chains = 3,
                      model_file = "radon.1.bug")

print radon_1
