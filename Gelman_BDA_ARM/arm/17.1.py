import rpy2.robjects as R
import rpy2.rinterface as rinterface
from rpy2.robjects.packages import importr
import rpy2.robjects.numpy2ri
from rpy2.robjects.vectors import FloatVector, StrVector
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

radon_data = StrVector(("n", "J", "x", "y", "county"))
radon_params = StrVector(("a", "b", "sigma.a", "sigma.b", "rho", "B", "mu.a", "mu.b", "tau.y", "sigma.y"))
radon_inits = R.r('''function () {
  list (B=array (rnorm (2*J), c(J,2)), sigma.y=runif(1), sigma.a=runif(1), 
        sigma.b=runif(1), rho=runif(1))
}''')

radon_4a = r2jags.jags(data = radon_data, inits = radon_inits,
                      parameters_to_save = radon_params,
                      n_iter = 10000,
                      n_chains = 3,
                      model_file = "radon.4a.bug")

print radon_4a
