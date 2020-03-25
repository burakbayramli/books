""" LDA.py """
from LDAmixture import *
from numpy.random import rand
from numpy.linalg import inv


fig = plt.figure()
plt.contourf(x, y,z, cmap=plt.cm.Blues, alpha= 0.9,extend='both')
plt.ylim(-5.0,8.0)
plt.xlim(-4.0,6.0)
M = 1000


r = (rand(M,1) < 0.5)
for i in range(0,M):
    if r[i]:
       u = np.random.multivariate_normal(mu1,Sigma,1)
       plt.plot(u[0][0],u[0][1],'.r',alpha = 0.4)
    else:
       u = np.random.multivariate_normal(mu2,Sigma,1)
       plt.plot(u[0][0],u[0][1],'+k',alpha = 0.6)

a = 2*inv(Sigma) @ (mu2-mu1);
b = ( mu1.reshape(1,2) @ inv(Sigma) @ mu1.reshape(2,1)  
    - mu2.reshape(1,2) @ inv(Sigma) @mu2.reshape(2,1) )
xx = np.linspace(-4,6,100)
yy = (-(a[0]*xx +b)/a[1])[0]
plt.plot(xx,yy,'m')

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plt.savefig('LDA2py.pdf',format='pdf')
plt.show()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%