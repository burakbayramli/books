import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import scipy.linalg as lin

clight = 299792458
xtrue = [3507884.948, 780492.718, 5251780.403, 0]

xxyyzz = [[16577402.072, 5640460.750, 20151933.185],
	  [11793840.229, -10611621.371, 21372809.480],
	  [20141014.004, -17040472.264, 2512131.115],
	  [22622494.101, -4288365.463, 13137555.567],
	  [12867750.433, 15820032.908, 16952442.746],
	  [-3189257.131, -17447568.373, 20051400.790],
	  [-7437756.358, 13957664.984, 21692377.935]]

xxyyzz = np.array(xxyyzz)

pseudorange = np.array([20432524.0, 21434024.4, 24556171.0, 21315100.2, 21255217.0, 24441547.2, 23768678.3])

l = pseudorange

xx = xxyyzz[:,0]
yy = xxyyzz[:,1]
zz = xxyyzz[:,2]
x = [0, 0, 0, 0]
A = np.ones((len(xxyyzz),4))
for iter in range(20):
    range = np.sqrt((x[0]-xx)**2+(x[1]-yy)**2+(x[2]-zz)**2);
    prange = range + x[3]
    F = prange
    irange = 1/range;
    dF = irange * (x[0]-xx)
    A[:,0] = dF
    dF = irange*(x[1]-yy);
    A[:,1] = dF
    dF = irange*(x[2]-zz);
    A[:,2] = dF
    k = l-F
    N = A.T;
    c = np.dot(N,k)
    N = np.dot(N,A)
    deltahat = lin.solve(N,c)
    x = x+deltahat
    if np.max(np.abs(deltahat))<0.001: break

print iter
print x






