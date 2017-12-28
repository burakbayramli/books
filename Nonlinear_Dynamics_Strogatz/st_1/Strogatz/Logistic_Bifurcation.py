import numpy as np
import matplotlib.pyplot as plt
import time as tm

plt.close()

def logistic_func(r,x):
  return r*x*(1-x)

def xnext(r,xo,N):
  no = 0
  X = np.zeros(r.size*N)
  A = np.zeros(r.size*N)
#Iterate for each value of r in the array of total_of_r (7000) values.
  for r_value in r:
    xinit = xo
#For each specific r value, iterate 200 x to get a starting value of X and A.
    for i in range(200):
      xinit = logistic_func(r_value,xinit)
    X[no] = xinit
    A[no] = r_value
    no += 1
#Now, for that r-value, iterate it N-times through the function.
#This gives N-values for each r-value.
    for j in range(1,N):
      X[no] = logistic_func(r_value,X[no-1])
      A[no]= r_value
      no += 1
  return A, X
  
#Using Broadcasting...not quite there.
#==============================
# Y = np.ones((1,1000))*.x0
# R = np.linspace(2.5,3.9,1000)
# for i in range(500):
#     Y = np.append(Y,logistic_func(R,Y[-10:]))
#     R = np.append(R,R[-1000:])
# plt.plot(R[-4500:],Y[-4500:],'.')
#==============================

xo = .4
Num = 71
r_min = 2.899
r_max = 3.999
total_of_r = 1700
r = np.linspace(r_min,r_max,total_of_r)

tic = tm.time()
A, X = xnext(r,xo,Num)
toc = tm.time()
print('elapsed time: %.4f seconds' % (toc-tic))

fig, ax = plt.subplots(figsize=(12,8))
ax.plot(A,X,".",markersize=0.1,color="green")
ax.set_xlim(r_min,r_max)
ax.set_ylim(0,1)
ax.set_title(r'Bifurcation plot of Logistic function: $Y_{next} = Y_{prior}*r*(1-Y_{prior})$')
ax.set_xlabel('r-value in equation')
ax.set_ylabel(r'$Y[r,Y_{prior}]$')
#ax.set_ax*.4is_off()
plt.gca().set_aspect('equal', adjustable='box')
plt.show()
plt.savefig('Bifur_plot.png')
# #Examine individual rows of N iterations.
# X_matrix = X.reshape(int(len(X)/Num),Num)
# plt.plot(X_matrix[900,:],'b.')
# plt.show()