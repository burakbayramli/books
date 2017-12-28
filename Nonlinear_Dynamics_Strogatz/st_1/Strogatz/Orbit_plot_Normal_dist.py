import numpy as np
import matplotlib.pyplot as plt
import time as tm

def logistic_func(r,x):
  return r*x*(1-x)

def normal(mu,sigma, x):
    norm_ = 1/np.sqrt(2*np.pi*sigma*sigma)
    return norm_*np.exp(-(x-mu)**2/(2*sigma*sigma))

def xnext(r,xo,N):
  no = 0
  X = np.zeros(r.size*N)
  A = np.zeros(r.size*N)
#Iterate for each value of r in the array of total_of_r (7000) values.
  for r_value in r:
    xinit = xo
#For each specific r value, iterate 200 x to get a starting value of X and A.
    for i in range(200):
      xinit = normal(0,r_value,xinit)
    X[no] = xinit
    A[no] = r_value
    no += 1
#Now, for that r-value, iterate it N-times through the function.
#This gives N-values for each r-value.
    for j in range(1,N):
      X[no] = normal(0,r_value,X[no-1])
      A[no]= r_value
      no += 1
  return A, X
#Fixed Values
#=========================
xo = 1.4
Num = 400
r_min = -1
r_max = 1
total_of_r = 20000
r = np.linspace(r_min,r_max,total_of_r)
#+++++++++++++++++++++++++

A, X = xnext(r,xo,Num)
#Plot of results
#+++++++++++++++++++++++++
fig, ax = plt.subplots(figsize=(8,8))
ax.set_facecolor(plt.cm.gray(.9))
ax.plot(A,X,",",markersize=0.1,color="green")
ax.set_xlim(r_min,r_max)
ax.set_ylim(-1,10)
ax.set_title(r'Bifurcation plot of Normal function: $x_{n + 1} = N(\mu, \sigma, \ x_n)$')
ax.set_xlabel(r'$\sigma$', size = 14)
ax.set_ylabel(r'$f( \mu, \sigma, \ x_n )$',size=14)
plt.show()
#plt.savefig('Bifur_plot.png')
