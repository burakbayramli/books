""" bvnormal.py """
import numpy as np
from numpy.random import randn
import matplotlib.pyplot as plt
N = 1000
r = 0.0 #change to 0.8 for other plot
Sigma = np.array([[1, r], [r, 1]])
B = np.linalg.cholesky(Sigma)
x = B @ randn(2,N)
plt.scatter([x[0,:]],[x[1,:]], alpha =0.4, s = 4)