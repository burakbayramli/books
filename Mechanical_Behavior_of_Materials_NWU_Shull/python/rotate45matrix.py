import numpy as np

sig = np.zeros((3,3)) # create stress tensor
sig[0][0] = 5e6 # only nonzero components
phi = 45
theta = np.array([[phi,90+phi,90],[90-phi,phi,90],[90,90,0]])
Q = np.cos(np.radians(theta))
QT = np.transpose(Q)
sigp = np.round(Q@sig@QT) #need to use appropiate code to do matrix multiplication in python
print(sigp)