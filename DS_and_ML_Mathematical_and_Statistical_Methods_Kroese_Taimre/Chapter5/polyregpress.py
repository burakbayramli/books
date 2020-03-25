""" polyregpress.py """
import numpy as np
from numpy.random import rand, randn
from numpy.linalg import norm, solve
import matplotlib.pyplot as plt
from sklearn.preprocessing import PolynomialFeatures

def generate_data(beta , sig, n):
    u = np.random.rand(n, 1)
    y = u ** np.arange(0, 4) @ beta.reshape((4,1)) + sig * np.random.randn(n, 1)
    return u.reshape((n,)), y.reshape((n,))

np.random.seed(12)
beta = np.array([10, -140, 400, -250]);
sig=5; n = 10**2;
u,y = generate_data(beta,sig,n)

K = 12 #maximum number of parameters
press = np.zeros(K)
for k in range(K):
    poly = PolynomialFeatures(k)
    X = poly.fit_transform(u.reshape(-1, 1)) # construct the model matrix
    P = X @ (np.linalg.inv(X.T @ X) @ (X.T)) # hat matrix
    e = y - P @ y

    press[k] = np.mean(np.power(np.divide(e,(1-np.diag(P))),2))

plt.plot(press) 