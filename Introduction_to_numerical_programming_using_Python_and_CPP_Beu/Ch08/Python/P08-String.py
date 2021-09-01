# Normal modes of a loaded string with fixed ends
from eigsys import *
from graphlib import *

n = 100                                              # number of point masses
a = [[0]*(n+1) for i in range(n+1)]                      # coefficient matrix
x = [[0]*(n+1) for i in range(n+1)]            # eigenvectors = displacements
d = [0]*(n+1)                                   # eigenvalues ~ frequencies^2
xp = [0]*(n+1)                                     # mesh points for plotting
yp = [0]*(n+1)                                 # function values for plotting

for i in range(1,n+1): a[i][i] = 2e0               # build coefficient matrix
for i in range(2,n-1): a[i+1][i] = -1e0; a[i][i+1] = -1e0

Jacobi(a,x,d,n)                                    # solve eigenvalue problem
EigSort(x,d,n,1)                          # sort eigenvalues and eigenvectors

GraphInit(1200,600)

h = 1e0/(n-1)
for i in range(1,n+1): xp[i] = (i-1) * h           # mesh points for plotting

mode = 1                                                      # normal mode 1
omega = sqrt(d[mode])*(n-1)                                       # frequency
for i in range(1,n+1): yp[i] = x[i][mode]                     # displacements
title = "omega({0:d}) = {1:6.2f}".format(mode,omega)
Plot(xp,yp,n,"blue",3,0.10,0.45,0.60,0.90,"x","y",title)

mode = 2                                                      # normal mode 2
omega = sqrt(d[mode])*(n-1)                                       # frequency
for i in range(1,n+1): yp[i] = x[i][mode]                     # displacements
title = "omega({0:d}) = {1:6.2f}".format(mode,omega)
Plot(xp,yp,n,"blue",3,0.60,0.95,0.60,0.90,"x","y",title)

mode = 3                                                      # normal mode 3
omega = sqrt(d[mode])*(n-1)                                       # frequency
for i in range(1,n+1): yp[i] = x[i][mode]                     # displacements
title = "omega({0:d}) = {1:6.2f}".format(mode,omega)
Plot(xp,yp,n,"blue",3,0.10,0.45,0.10,0.40,"x","y",title)

mode = 4                                                      # normal mode 4
omega = sqrt(d[mode])*(n-1)                                       # frequency
for i in range(1,n+1): yp[i] = x[i][mode]                     # displacements
title = "omega({0:d}) = {1:6.2f}".format(mode,omega)
Plot(xp,yp,n,"blue",3,0.60,0.95,0.10,0.40,"x","y",title)

MainLoop()
