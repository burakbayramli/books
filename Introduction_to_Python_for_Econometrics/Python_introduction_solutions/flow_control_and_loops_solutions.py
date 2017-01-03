"""Solutions for 'Flow Control and Loops' chapter.  

Solutions file used IPython demo mode.  To play, run

from IPython.lib.demo import Demo
demo = Demo('flow_control_and_loops_solutions.py')
 
and then call 

demo()

to play through the code in steps.
"""
# <demo> auto
from __future__ import print_function
from numpy import zeros, ones, absolute
from numpy.random import randn
import scipy.stats as stats
# <demo> --- stop ---
# Exercise 1
r1 = randn(1)
r2 = randn(1)

if r1<0 and r2<0: #NOPRINT
    pass #NOPRINT
elif (r1>=0 and r2<0) or (r1<0 and r2>=0): #NOPRINT
    pass #NOPRINT
else: #NOPRINT
    pass #NOPRINT
# <demo> --- stop ---
# Exercise 2
T = 1000
e = randn(2*T)
phi1 = 1.4
phi2 = -.8
theta1 = 0.4
theta2 = 0.8

y = zeros(e.shape)
for t in xrange(2,2*T):
    y[t] = phi1*y[t-1] + phi2*y[t-2] + theta1*e[t-1] + theta2*e[t-2] + e[t]
    
y = y[1000:]

# <demo> --- stop ---
# Exercise 3
omega = 0.05 
alpha = 0.05
beta  = 0.9

y = zeros(2*T)
sigma2 = ones(2*T)
for i in xrange(1,2*T):
    sigma2[t] = omega + alpha*(y[t-1]**2) + beta*sigma2[t-1]
    y[t] = sigma2[t]*e[t]
    
sigma2 = sigma2[1000:]
y = y[1000:]
# <demo> --- stop ---
# Exercise 4
omega = 0.05 
alpha = 0.02
gamma = 0.07
beta  = 0.9

y = zeros(2*T)
sigma2 = ones(2*T)
for i in xrange(1,2*T):
    sigma2[t] = omega + alpha*(y[t-1]**2) + gamma*((y[t-1]<0)*y[t-1]**2) + beta*sigma2[t-1]
    y[t] = sigma2[t]*e[t]
    
sigma2 = sigma2[1000:]
y = y[1000:]
# <demo> --- stop ---
# Exercise 6
M = 5
x = zeros((M,M))
for i in xrange(M):
    for j in xrange(M):
        x[i,j] = i * j

y = zeros((M,M))
for i,row in enumerate(y):
    for j,col in enumerate(row):
        y[i,j] = i * j
# <demo> --- stop ---
# Exercise 7

def invert_normal_cdf(prob):
    UB = 10.0
    LB = -10.0
    cdf = stats.norm(loc=0,scale=1).cdf 
    distance = 1
    while distance > 1e-8:
        MP = (UB+LB)/2
        MPc = cdf(MP)
        if prob>MPc:
            LB = MP
        else:
            UB = MP
        distance = absolute(MPc-prob)
    
    return MP

# <demo> --- stop ---
# Exercise 8    

icdf = stats.norm(loc=0,scale=1).ppf

print("invert_normal_cdf(.01):")
print(invert_normal_cdf(.01))
print("icdf(.01):")
print(icdf(.01))
print("invert_normal_cdf(.5):")
print(invert_normal_cdf(.5))
print("icdf(.5):")
print(icdf(.5))
print("invert_normal_cdf(.975)  :")
print(invert_normal_cdf(.975)  )
print("icdf(.975):")
print(icdf(.975))


# <demo> --- stop ---
# Exercise 9
r = randn(1000)
y = [val for val in r if val<0]
print("r[r<0]:")
print(r[r<0])
