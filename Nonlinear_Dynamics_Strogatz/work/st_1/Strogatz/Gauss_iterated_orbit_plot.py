import numpy as np
import matplotlib.pyplot as plt
import time as tm

def normal(alpha,beta,x):
    return np.exp(-alpha*x*x) + beta

def xnext(r,xo,N):
  no = 0
  X = np.zeros(r.size*N)
  A = np.zeros(r.size*N)
#Iterate for each value of r in the array of total_of_r (7000) values.
  for beta in r:
    xinit = xo
#For each specific r value, iterate 200 x to get a starting value of X and A.
    for i in range(200):
      xinit = normal(alpha,beta,xinit)
    X[no] = xinit
    A[no] = beta
    no += 1
#Now, for that r-value, iterate it N-times through the function.
#This gives N-values for each r-value.
    for j in range(1,N):
      X[no] = normal(alpha,beta,X[no-1])
      A[no]= beta
      no += 1
  return A, X

alpha = 4.9
xo = 1.4
Num = 400
beta_min = -1
beta_max = 1
total_of_r = 100000
r = np.linspace(beta_min,beta_max,total_of_r)

A, X = xnext(r,xo,Num)

fig, ax = plt.subplots(figsize=(12,8))
ax.plot(A,X,",",markersize=0.1,color="green")
ax.set_xlim(beta_min,beta_max)
ax.set_ylim(-1,1.5)
ax.set_title(r'Bifurcation plot of Gauss iterated function: $ x_{n + 1} = e^{-\alpha.x_n^2} + \beta$')
ax.set_xlabel('sigma-value in equation')
ax.set_ylabel(r'$Y[r,Y_{prior}]$')
plt.show()
#plt.savefig('Gauss_iterate_map_orbit_plot.png')
