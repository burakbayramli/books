""" MCsim.py """
import numpy as np
import matplotlib.pyplot as plt
n = 101
P = np.array([[0, 0.2, 0.5, 0.3],
                [0.5, 0, 0.5, 0],
                [0.3, 0.7, 0, 0],
                [0.1, 0, 0, 0.9]])
x = np.array(np.ones(n, dtype=int))
x[0] = 0
for t in range(0,n-1):
    x[t+1] = np.min(np.where(np.cumsum(P[x[t],:])> np.random.rand()))  
x = x + 1  #add 1 to all elements of the vector x
plt.plot(np.array(range(0,n)),x, 'o')
plt.plot(np.array(range(0,n)),x, '--')
plt.show()