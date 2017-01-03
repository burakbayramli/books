"""Solutions for 'Special Arrays' chapter.  

Solutions file used IPython demo mode.  To play, run

from IPython.lib.demo import Demo
demo = Demo('special_arrays_solutions.py')
 
and then call 

demo()

to play through the code in steps.
"""
# <demo> auto
from __future__ import print_function
from numpy.random import uniform, normal, gamma, lognormal, randn
import scipy.stats  as stats
import numpy as np
import matplotlib.pyplot as plt
# <demo> --- stop ---
# Exercise 1
a = normal(0,1,size=(1000,))
b = normal(3,3,size=(1000,))
c = uniform(0,1,size=(1000,))
d = uniform(-1,1,size=(1000,))
e = gamma(1,2,size=(1000,))
f = lognormal(.08,.2,size=(1000,))
# <demo> --- stop ---
# Exercise 2
stats.kstest(a, stats.norm(loc=0,scale=1).cdf)
stats.kstest(b, stats.norm(loc=3,scale=3).cdf)
stats.kstest(c, stats.uniform(loc=0,scale=1).cdf)
stats.kstest(d, stats.uniform(loc=-1,scale=2).cdf)
stats.kstest(e, stats.gamma(1, scale=2).cdf)
# <demo> --- stop ---
# Exercise 3
x = randn(1)
print("np.random.seed():")
print(np.random.seed())
x = randn(1)
# <demo> --- stop ---
# Exercise 4
state = np.random.get_state()
x = randn(2,1)
print("x:")
print(x)
print("np.random.set_state(state):")
print(np.random.set_state(state))
x = randn(2,1)
print("x:")
print(x)
# <demo> --- stop ---
# Exercise 5

def summary_stats(x):
    return x.mean(), x.std(), stats.skew(x), stats.kurtosis(x)+3
    
    
x = randn(100)
print("summary_stats(x):")
print(summary_stats(x))
# <demo> --- stop ---
# Exercise 6
x = randn(100,2)
rho = 0.5
x[:,1] = rho * x[:,0] + np.sqrt(1-rho**2) * x[:,1]

print("stats.pearsonr(x[:,0],x[:,1]):")
print(stats.pearsonr(x[:,0],x[:,1]))
print("stats.spearmanr(x[:,0],x[:,1]):")
print(stats.spearmanr(x[:,0],x[:,1]))
print("stats.kendalltau(x[:,0],x[:,1]):")
print(stats.kendalltau(x[:,0],x[:,1]))
# <demo> --- stop ---
# Exercise 7
gam = stats.gamma(1,scale=2)
print("gam.median():")
print(gam.median())
print("np.median(gam.rvs(10000)):")
print(np.median(gam.rvs(10000)))
# <demo> --- stop ---
# Exercise 8
x = np.linspace(0,1-(1.0/1000),1000) + 1.0/2000
plt.figure(1) #NOPRINT
u = np.sort(stats.norm(loc=0,scale=1).cdf(a))
plt.plot(x,u,x,x) #NOPRINT
plt.figure(2) #NOPRINT
u = np.sort(stats.norm(loc=3,scale=3).cdf(b))
plt.plot(x,u,x,x) #NOPRINT
plt.figure(3) #NOPRINT
u = np.sort(stats.uniform(loc=0,scale=1).cdf(c))
plt.plot(x,u,x,x) #NOPRINT
plt.figure(4) #NOPRINT
u = np.sort(stats.uniform(loc=-1,scale=2).cdf(d))
plt.plot(x,u,x,x) #NOPRINT
plt.figure(5) #NOPRINT
u = np.sort(stats.gamma(1, scale=2).cdf(e))
plt.plot(x,u,x,x) #NOPRINT
plt.show() #NOPRINT