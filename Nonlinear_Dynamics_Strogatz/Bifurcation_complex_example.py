#https://gist.github.com/hamfat/6ed873c47b2f2033d69ad2c7cdcb930a
import numpy as np
from scipy import integrate
import sympy as sm
import matplotlib.pyplot as plt
#%matplotlib inline




def func(Y,t,v1,v2,v4,v5,v6,ca3,ca4,phi,vl,vk,vca,gk,gl,gc,c,kd,bt,alpha,kca):
    #n =Y[0], v =Y[1] , ca =Y[2]
    v3=(-v5/2)*np.tanh((Y[2]-ca3)/ca4)+v6
    return np.array([phi*np.cosh((Y[1]-v3)/(2*v4))*(0.5*(1+np.tanh((Y[1]-v3)/v4))-Y[0]),
                     1/c*(-gl*(Y[1]-vl)-gk*Y[0]*(Y[1]-vk)-gc*0.5*(1+np.tanh((Y[1]-v1)/v2))*(Y[1]-vca)),
                    ((-alpha*gc*0.5*(1+np.tanh((Y[1]-v1)/v2))-kca*Y[2])*((kd+Y[2])**2/((kd+Y[2])**2+kd*bt)))])

# to generate the x-axes
t= np.linspace(0,10,1000)

#initial values

func0= [0,0, 0]  # [N,V,CA]

pars =  (-0.0275,0.025,0.0145,0.008,-0.015,4.0e-7,1.5e-7,2.664,-0.07,-0.09,0.08,3.1416e-13,7.854e-14,1.57e-13,1.9635e-14,1.0e-6,1.0e-4,7.9976e7,1.3567537e2)

Y = integrate.odeint(func, func0, t, pars)

n,v,ca = Y.T

# the plots


plt.subplot(4,1,1)
plt.plot(t, n, 'r', linewidth=2,label='n')
plt.xlabel('t')
plt.ylabel('n(t)')
plt.legend(loc='best')


plt.subplot(4,1,2)
plt.plot(t, v, 'b',linewidth=2, label='v')
plt.xlabel('t')
plt.ylabel('v(t)')
plt.legend(loc='best')


plt.subplot(4,1,3)
plt.plot(t,ca, 'g',linewidth=2, label='ca')
plt.xlabel('t')
plt.ylabel('ca(t)')
plt.legend(loc='best')

plt.subplot(4,1,4)
plt.plot(n,v, 'b',linewidth=2, label='ca')
plt.xlabel('v')
plt.ylabel('n')
plt.legend(loc='best')




######################################################################################################################

# to store the max_min of the solutions
Ymin = []
Ymax = []
V_1 = np.linspace(-0.0296, -0.0166, 100)
t = np.linspace(0, 100,1000)

for v1 in V_1:
    pars = (v1,0.025,0.0145,0.008,-0.015,4.0e-7,1.5e-7,2.664,-0.07,-0.09,0.08,3.1416e-13,7.854e-14,1.57e-13,1.9635e-14,1.0e-6,1.0e-4,7.9976e7,1.3567537e2)

    # integrate again the equation, with new parameters
    Y = integrate.odeint(func, func0, t, pars)

    # appending the result to the list
    Ymin.append(Y[-60:,:].min(axis=0))
    Ymax.append(Y[-60:,:].max(axis=0))

# convert the lists into arrays
Ymin = np.asarray(Ymin)
Ymax = np.asarray(Ymax)


# plot the bifurcation diagram
plt.figure()

plt.subplot(3,1,1)
plt.plot(V_1, Ymin[:,0], 'r', linewidth=2,label='n')
plt.plot(V_1, Ymax[:,0], 'r',linewidth=2)
plt.xlabel('$v1$')
plt.ylabel('n')
plt.legend(loc='best')


plt.subplot(3,1,2)
plt.plot(V_1, Ymin[:,1], 'b',linewidth=2, label='v')
plt.plot(V_1, Ymax[:,1], 'b', linewidth=2)
plt.xlabel('$v1$')
plt.ylabel('v')
plt.legend(loc='best')


plt.subplot(3,1,3)
plt.plot(V_1, Ymin[:,2], 'g',linewidth=2, label='ca')
plt.plot(V_1, Ymax[:,2], 'g',linewidth=2)
plt.xlabel('$v1$')
plt.ylabel('ca')
plt.legend(loc='best')

plt.show()
