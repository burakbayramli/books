# Real roots of a polynomial by the Birge-Vieta method
from math import *
from roots import *

n = 4                                                  # degree of polynomial
ax = [1e0, 0e0, -5e0, 0e0, 4e0]                                # coefficients
xx = [0]*(n+1)                                                        # zeros

nx = BirgeVieta(ax,n,xx)                                     # nx zeros found

for i in range(1,nx+1): print("x[",i,"] = ",xx[i])
if (nx <= 0): print(" Non-existing real zeros")
