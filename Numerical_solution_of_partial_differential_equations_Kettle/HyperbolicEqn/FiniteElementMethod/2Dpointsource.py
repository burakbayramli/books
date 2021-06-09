## 2Dpointsource.py

## Author: Louise Olsen-Kettle (email: l.kettle1@uq.edu.au, @DrOlsen-Kettle)
## Code to supplement lecture notes by Louise Olsen-Kettle:
## Numerical solution of Partial Differential Equations (PDEs) 
## Webpage: http://espace.library.uq.edu.au/view/UQ:239427
## ISBN: 978-1-74272-149-1  
## this code needs eScript installed to run, dowload for free from https://launchpad.net/escript-finley 
## also available on School of Earth Science, UQ's supercomputer Savanna
## The ouput can be visualised using VisIt which can be downloaded for free from https://visit.llnl.gov/ 


# we are solving the P-wave equation with a point source
# there are no absorbing boundary conditions at the grid edges so the 
# waves are reflected at the boundary

from esys.escript import *
from esys.escript.linearPDEs import LinearPDE
from esys.finley import Rectangle 
import numpy

#constants:
lam=3.462e9
mu=3.462e9
rho=1154.
#time problem solved for:
tend=4.
#initial displacement at point source:
U0=0.5

# setting up the finite element mesh, domain
nx_0= 100      # number of elements in x_0 direction
nx_1= 100       # number of elements in x_1 direction
width=10000.  # length in x_0 and x_1 directions

l_0=width  # length of domain in x_0 direction
l_1=width  # length of domain in x_1 direction

domain=Rectangle(nx_0,nx_1,l0=l_0,l1=l_1)

# this sets up a 2D finite element mesh (for a 3D mesh use Brick) 
# in wave propagation a regular mesh should be used where the elements
# are the same size in all directions 
# dx_0 = l0/nx_0, dx_1 = l1/nx_1  should be equal. 


# setting safe times step using Courant condition: h = (1/5)*(dx/Vp)
# h is the safe time step
# Vp = sqrt((lam+2*mu)/rho) is the P-wave speed
# dx = width/nx_0  is the size of each mesh element

Vp = sqrt((lam+2*mu)/rho)

h=(1./5.)*(width/nx_0)/Vp
print "time step size = ",h


x=domain.getX()
# ... open new PDE class in escript ...
mypde=LinearPDE(domain)

# location of source in middle of domain

xc=[width/2.,width/2.]

# define small radius around point xc for point source
# Lsup(x) returns the maximum absolute value of the argument x
src_radius = 0.1*Lsup(domain.getSize())
print "src_radius = ",src_radius

# in escript to set values of coefficients in PDE use setValue:
mypde.setValue(D=1)
# D is the coefficient in PDE: 1*acceleration
# where we are solving for acceleration. 

# ... set initial values ....
n=0

# initial value of displacement is zero
# for first two time steps
u=x[0]-x[0]
u_last=x[0]-x[0]
t=0

# define the PointForce function
def PointForce(a,b,U0,t):
  P=-2*(U0*sqrt(b/numpy.pi))*b*(t-a)*exp(-b*(t-a)**2)
  return P

# now we need to solve our Linear PDE for acceleration at each time step:
# we introduce a loop over time: from t = 0 until t = t_end 

# counter for images:
icount=-1

# To save displacement
dm = DataManager(formats=[DataManager.VTK])
dm.addData(displacement=u)
dm.setTime(t)
dm.export()

while t<tend:
  # ... get u,j at last time step  ....
  g=grad(u)
  # for a scalar u: g[i] = grad(u) = du/dx[j] = u,j 

  # set a smooth point source at x=xc
  F=PointForce(3,1,U0,t)*whereNegative(length(x-xc)-src_radius)

  # set Y coefficient in escript for PDE for acceleration:
  mypde.setValue(Y=F)

  # set X coefficient in escript for PDE for acceleration:
  mypde.setValue(X=-(Vp**2)*g)

  # ... get new acceleration ....
  a=mypde.getSolution()

  # ... get new displacement ...
  # update the displacement for new (n+1)th time step (u(n+1)) using a Verlet time integration scheme:
  u_new=2*u-u_last+h**2*a

  # update displacements at current time step 
  u_last=u
  u=u_new
  # march forward in time:
  t+=h
  # march counter (n) forward too:
  n+=1

  # ... save current acceleration and displacements every 10th time step 
  if n%10==0:
     dm.setTime(t)
     dm.addData(displacement=u)
     dm.export()



 
