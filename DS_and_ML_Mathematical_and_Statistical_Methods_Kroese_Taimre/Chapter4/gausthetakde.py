""" gausthetakde.py """

import matplotlib.pyplot as plt
import numpy as np
from kde import *

np.random.seed(1234)
h   = 0.1; h2 = h**2; c=1/np.sqrt(2*np.pi)/h #Constants
phi = lambda x,x0: np.exp(-(x-x0)**2/(2*h2)) #Unscaled Kernel
f   = lambda x: np.exp(-x)*(x >= 0) # True PDF 
n = 10**4 # Sample Size
x = -np.log(np.random.uniform(size=n))# Generate Data via IT method
xx = np.arange(-0.5,6,0.01, dtype = "d")# Plot Range
phis = np.zeros(len(xx))
for i in range(0,n):
    phis = phis + phi(xx,x[i])
phis = c*phis/n

plt.figure(figsize=[6,3])

plt.plot(xx,phis,'r')# Plot Gaussian KDE

[bandwidth,density,xmesh,cdf] = kde(x,2**12,0,max(x))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
idx = (xmesh <= 6)
plt.plot(xmesh[idx],density[idx],'b')# Plot Theta KDE
plt.plot(xx,f(xx),'k--')# Plot True PDF
plt.legend(['Gaussian KDE','Theta KDE', 'True density'])
plt.savefig('gausthetakde.pdf',format='pdf')
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
