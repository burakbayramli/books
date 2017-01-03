import rpy2.robjects as R
import rpy2.rinterface as rinterface
from rpy2.robjects.packages import importr
import rpy2.robjects.numpy2ri
from rpy2.robjects.vectors import FloatVector, StrVector
import numpy as np
r2jags = importr('R2jags')
mcmcpack = importr('MCMCpack')

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

X = np.ones((len(x), 2))
X[:,1] = x
K = X.shape[1]
W = np.eye(K)

R.r.assign('n',n)
R.r.assign('J',J)
R.r.assign('y',y)
R.r.assign('county',county)
R.r.assign('x',x)
R.r.assign('W',W)
R.r.assign('K',K)
R.r.assign('X',X)

radon_data = StrVector(("n", "J", "K", "X", "y", "county", "W"))
radon_params = StrVector(("B", "mu", "sigma.y", "sigma.B", "rho.B"))
radon_inits = R.r('''function (){
  list (B.raw=array(rnorm(J*K), c(J,K)), mu.raw=rnorm(K), sigma.y=runif(1),
       Tau.B.raw=rwish(K+1,diag(K)), xi=runif(K))
}''')

radon_4a = r2jags.jags(data = radon_data, inits = radon_inits,
                      parameters_to_save = radon_params,
                      n_iter = 2000,
                      n_chains = 3,
                      model_file = "wishart2.bug")

print radon_4a
