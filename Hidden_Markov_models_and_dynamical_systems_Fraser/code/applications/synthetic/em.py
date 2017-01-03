''' em.py makes data for Fig. 2.7, fig:GaussMix, of the book
'''

Copyright = '''
Copyright 2005 Andrew M. Fraser, and 2013 Andrew M. Fraser and Los
Alamos National Laboroatory

This file is part of hmmds3.

Hmmds3 is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

Hmmds3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

See the file gpl.txt in the root directory of the hmmds3 distribution
or see <http://www.gnu.org/licenses/>.
'''
import random, sys
from math import sqrt, pi, exp

out_file = sys.argv[1]
if len(sys.argv) == 3 and sys.argv[2] == 'print':
   flag_p = True
else:
   flag_p = False

def _print(*args, **kwargs):
   if flag_p:
      print(*args, **kwargs)

random.seed(3)

mu = [-2.0,2.0] # Means of the two components
Y = []          # To hold the sequence of observations
T = 10          # Number of observations
em_iterations = 2

# Create the data
_print('# Here are the observations')
for t in range(T):
   s = random.randint(0,1)       # Choose randomly 0 or 1
   y = random.gauss(mu[s],1.0)   # Draw from \Normal(mu[s], 1)
   Y.append(y)
_print((T*'%5.2f ')%tuple(Y))

# Initial model parameters
alpha = [0.5]
mu_i = [[-1.0, 1.0]]

for i in range(em_iterations):
   _print('i=%d alpha=%6.3f mu0=%6.3f mu1=%6.3f\n'%(i, alpha[i], mu_i[i][0],
                                                   mu_i[i][1]))
   ps = []     # State probabilities
   sums = 0.0  # Sum_t prob(class_0|Y[t])
   # Estimate state probabilities for each observation
   for t in range(T):
      p0t = (alpha[i]/sqrt(2*pi)) * exp((-(Y[t]-mu_i[i][0])**2)/2)
      p1t = ((1-alpha[i])/sqrt(2*pi)) * exp((-(Y[t]-mu_i[i][1])**2)/2)
      ps.append(p0t/(p0t + p1t))
      sums += ps[t]
   _print('w(t)        ', (len(ps)*'%5.2f ')%tuple(ps))
   _print('w(t)y(t)    ', (len(ps)*'%5.2f ')%tuple(Y[t]*ps[t]
                                                  for t in range(T)))
   _print('(1-w(t))y(t)', (len(ps)*'%5.2f ')%tuple(Y[t]*(1-ps[t])
                                                  for t in range(T)))
   # Reestimate model parameters mu_i and alpha
   sum0 = 0.0
   sum1 = 0.0
   for t in range(T):
      sum0 += Y[t]*ps[t]
      sum1 += Y[t]*(1.0-ps[t])
   alpha.append(sums/T)
   mu_i.append([sum0/sums, sum1/(T-sums)])
_print('i=%d alpha=%6.3f mu0=%6.3f mu1=%6.3f\n'%(
   em_iterations, alpha[-1], mu_i[-1][0], mu_i[-1][1]))

mu_i.append(mu)    # Record initial model
alpha.append(0.5)  # Record initial model
import pickle
pickle.dump({'Y':Y, 'alpha':alpha, 'mu_i':mu_i}, open(out_file,'wb'),
            protocol=2)

#Local Variables:
#mode:python
#End:
