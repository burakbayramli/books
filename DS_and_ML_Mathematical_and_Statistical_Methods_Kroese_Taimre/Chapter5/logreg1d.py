""" logreg1d.py """
import numpy as np
import matplotlib.pyplot as plt
from  numpy.linalg import lstsq

n = 100                                      # sample size
x = (2*np.random.rand(n)-1).reshape((n,1))  # explanatory variables
beta = np.array([-3, 10])
Xmat = np.hstack((np.ones((n,1)), x))
p = 1/(1 + np.exp(-Xmat @ beta))
y = np.random.binomial(1,p,n)                # response variables

# initial guess
betat = lstsq((Xmat.T @ Xmat),Xmat.T @ y, rcond=None)[0]

grad = np.array([2,1])                             # gradient 

while (np.sum(np.abs(grad)) > 1e-5) :      # stopping criteria
    mu = 1/(1+np.exp(-Xmat @ betat))
    # gradient
    delta = (mu - y).reshape((n,1))
    grad = np.sum(np.multiply( np.hstack((delta,delta)),Xmat), axis=0).T
    # Hessian
    H = Xmat.T @ np.diag(np.multiply(mu,(1-mu))) @ Xmat    
    betat = betat - lstsq(H,grad,rcond=None)[0]
    print(betat)
    
plt.plot(x,y, '.') # plot data

xx = np.linspace(-1,1,40).reshape((40,1))
XXmat = np.hstack( (np.ones((len(xx),1)), xx)) 
yy = 1/(1 + np.exp(-XXmat @ beta))
plt.plot(xx,yy,'r-')                      #true logistic curve
yy = 1/(1 + np.exp(-XXmat @ betat));
plt.plot(xx,yy,'k--')                               # estimated curve




     