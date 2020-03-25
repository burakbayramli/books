""" QDA.py """
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import multivariate_normal

mu1 = np.array([0,0])
mu2 = np.array([2,2])
Sigma1 = np.array([[1,0.3],[0.3, 1]])
Sigma2 = np.array([[0.3,0.3],[0.3, 1]])
x, y = np.mgrid[-2:4:150j,-3:5:150j]
mvn1 = multivariate_normal( mu1, Sigma1 )
mvn2 = multivariate_normal( mu2, Sigma2 )

xy = np.hstack((x.reshape(-1,1),y.reshape(-1,1)))
z = 0.5*mvn1.pdf(xy).reshape(x.shape) +  0.5*mvn2.pdf(xy).reshape(x.shape)
plt.contour(x,y,z)

z1 = 0.5*mvn1.pdf(xy).reshape(x.shape) -  0.5*mvn2.pdf(xy).reshape(x.shape)
plt.contour(x,y,z1, levels=[0],linestyles ='dashed', linewidths = 2, colors = 'm')

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plt.savefig('QDApy.pdf')
plt.show()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%