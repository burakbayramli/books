#From http://scipy.github.io/old-wiki/pages/Cookbook/LoktaVolterraTutorial
import numpy as np
import matplotlib.pyplot as plt
# Definition of parameters
a = 1.
b = 0.1
c = 1.5
d = 0.75
def dX_dt(X, t=0):
    """ Return the growth rate of fox and rabbit populations. 
        It appears that it needs to take a numpy array in, just one,
        and returns the same. """
    return np.array([ a*X[0] -   b*X[0]*X[1] ,
                  -c*X[1] + d*b*X[0]*X[1] ])
#Population equilibrium points when dX_dt = 0. 
X_f0 = np.array([     0. ,  0.])
X_f1 = np.array([ c/(d*b), a/b])

all(dX_dt(X_f0) == np.zeros(2) ) and all(dX_dt(X_f1) == np.zeros(2)) # => True

#Create the Jacobian for the purpose of evaluating eigenvalues etc. 
def d2X_dt2(X, t=0):
    """ Return the Jacobian matrix evaluated in X. """
    return np.array([[a -b*X[1],   -b*X[0]     ],
                  [b*d*X[1] ,   -c +b*d*X[0]] ])
#So near X_f0, which represents the extinction of both species, we have:
A_f0 = d2X_dt2(X_f0)        # >>> array([[ 1. , -0. ],
                                        #            [ 0. , -1.5]])

#Near X_f0, the number of rabbits increase and the population of foxes decrease. 
#The origin is therefore a saddle point.
#Near X_f1, we have:                                        
A_f1 = d2X_dt2(X_f1)                    # >>> array([[ 0.  , -2.  ],
                                        #            [ 0.75,  0.  ]])
# whose eigenvalues are +/- sqrt(c*a).j:
lambda1, lambda2 = np.linalg.eigvals(A_f1) # >>> (1.22474j, -1.22474j)
# They are imaginary numbers. The fox and rabbit populations are periodic as follows from further
# analysis. Their period is given by:
T_f1 = 2*np.pi/abs(lambda1)                # >>> 5.130199  

#Integrate the functions in terms of a starting point. 
from scipy import integrate
t = np.linspace(0, 15,  1000)              # time
X0 = np.array([10, 5])                     # initials conditions: 10 rabbits and 5 foxes
X, infodict = integrate.odeint(dX_dt, X0, t, full_output=True)
infodict['message']                     # >>> 'Integration successful.'
#Plot the evolution of both populations.
rabbits, foxes = X.T
fig, (ax0,ax1) = plt.subplots(2,1,figsize=(8,8))
ax0.plot(t, rabbits, 'r-', label='Rabbits')
ax0.plot(t, foxes  , 'b-', label='Foxes')
ax0.grid()
ax0.legend(loc='best')
ax0.set_xlabel('time')
ax0.set_ylabel('population')
ax0.set_title('Evolution of fox and rabbit populations')
#f1.savefig('rabbits_and_foxes_1.png')   
#Plot the phase space.
values  = np.linspace(0.3, 0.9, 5)                          # position of X0 between X_f0 and X_f1
vcolors = plt.cm.autumn_r(np.linspace(0.3, 1., len(values)))  # colors for each trajectory
#-------------------------------------------------------
# plot trajectories
for v, col in zip(values, vcolors): 
    X0 = v * X_f1                               # starting point
    X = integrate.odeint( dX_dt, X0, t)         # we don't need infodict here
    ax1.plot( X[:,0], X[:,1], lw=3.5*v, color=col, label='X0=(%.f, %.f)' % ( X0[0], X0[1]) )

#-------------------------------------------------------
# define a grid and compute direction at each point
ymax = ax1.set_ylim(ymin=0)[1]                        # get axis limits
xmax = ax1.set_xlim(xmin=0)[1] 
nb_points   = 20                      

x = np.linspace(0, xmax, nb_points)
y = np.linspace(0, ymax, nb_points)

X1 , Y1  = np.meshgrid(x, y)                       # create a grid
DX1, DY1 = dX_dt([X1, Y1])                      # compute growth rate on the gridt
M = (np.hypot(DX1, DY1))                           # Norm of the growth rate 
M[ M == 0] = 1.                                 # Avoid zero division errors 
DX1 /= M                                        # Normalize each arrows
DY1 /= M                                  

#-------------------------------------------------------
# Drow direction fields, using matplotlib 's quiver function
# I choose to plot normalized arrows and to use colors to give information on
# the growth speed
ax1.set_title('Trajectories and direction fields')
Q = ax1.quiver(X1, Y1, DX1, DY1, M, pivot='mid', cmap=plt.cm.jet)
ax1.set_xlabel('Number of rabbits')
ax1.set_ylabel('Number of foxes')
ax1.legend()
ax1.grid()
ax1.set_xlim(0, xmax)
ax1.set_ylim(0, ymax)
plt.show()
#f2.savefig('rabbits_and_foxes_2.png')                                                                         