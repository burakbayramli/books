import numpy as np
import scipy.integrate as integrate
from scipy.optimize import fsolve
import matplotlib.pyplot as plt


# returns the initial condition of the PDE
def initial_condition(z, alpha, beta):
	return alpha + beta*np.sin(z)

# nonlinear function f(xi) = x - x0 - beta*sin(x0)*t for method
# of characteristics
def myfun(x_past, x, t, beta):
	return x - x_past - beta*np.sin(x_past)*t

# apply nonlinear solve to obtain x0 in method of characteristics
def burgers(beta, x, t):

	# compute x0 using nonlinear solve
	x_next = np.zeros((len(x),1))

	for j in range(0,len(x)):
		x_next[j] = fsolve(myfun, 0.0, args=(x[j], t, beta))

	return beta*np.sin(x_next)

# returns exact solution of Burgers equation
def exact_solution(x, t, alpha, beta):

	x = np.asarray(x)

	# location of shock
	if (np.pi + alpha*t > 2.0*np.pi):
		d = pi + alpha*t - 2.0*pi*np.floor((pi + alpha*t)/(2.0*np.pi))
		x += 2.0*pi*np.floor((pi + alpha*t)/(2.0*np.pi))
	else:
		d = np.pi + alpha*t

	d2 = np.pi + alpha*t

	# determine whether x lies before or after the shock
	idx1 = np.all(x <= d2)
	idx2 = np.all(x > d2)

	x1 = x[idx1]
	x2 = x[idx2]

	if (x1.size != 0):
		# coordinate transformation
		xn = x1 - alpha*t

		# solve burgers using the coordinate transformation
		u = burgers(beta, xn, t)
		v = alpha + u

		x1 -= 2.0*np.pi*np.floor((np.pi+alpha*t)/(2.0*np.pi));

		if (x2.size != 0):
			x2 = np.flip(2.0*d2 - x2)
			xn2 = x2 - alpha*t

			u2 = burgers(beta, xn2, t)
			v2 = alpha + u2

			x2 -= 2.0*np.pi*floor((np.pi+alpha*t)/(2.0*np.pi))

			vv = np.append(v, np.flip(2.0*alpha-v2))
		else:
			vv = v

	elif (x2.size != 0):
		x2 = np.flip(2.0*d2 - x2)
		xn2 = x2 - alpha*t

		u2 = burgers(beta, xn2, t)
		v2 = alpha + u2

		x2 -= 2.0*np.pi*np.floor((np.pi+alpha*t)/(2.0*np.pi))
		vv = np.flip(2.0*alpha - v2)

	return vv

# flux function for the conservation law
#	u_t + f(u)_x = 0
#
def flux(u):
	return 0.5*u**2

# Roe numerical flux
def roe_flux(um, up):
	fm = flux(um)
	fp = flux(up)
	s = np.zeros((len(um),1))

	for i in range(0,len(um)):
		if (up[i] == um[i]):
			s[i] = um[i]
		else:
			s[i] = (fp[i]-fm[i])/(up[i]-um[i])

	return 0.5*(fm+fp - np.sign(s)*(fp-fm))

# Godunov numerical flux
def godunov_flux(um, up):
	fhat = np.zeros((len(um),1))

	for i in range(0,len(um)):
		if (um[i]*up[i]<0.0 and um[i]<=up[i]):
			fhat[i] = 0.0
		elif (um[i]*up[i]<0.0 and um[i]>up[i]):
			fhat[i] = np.fmax(flux(um[i]), flux(up[i]))
		elif (um[i]*up[i]>=0.0 and um[i]<=up[i]):
			fhat[i] = np.fmin(flux(um[i]), flux(up[i]))
		elif (um[i]*up[i]>=0.0 and um[i]>up[i]):
			fhat[i] = np.fmax(flux(um[i]), flux(up[i]))
	return fhat

# Lax-Friedrichs numerical flux
def lf_flux(um, up, A):
	fm = flux(um)
	fp = flux(up)

	return 0.5*( fm+fp - A*(up-um) )

# Engquist-Osher numerical flux
def eo_flux(um, up):
	fhat = np.zeros((len(um),1))

	for i in range(0,len(um)):
		if (um[i]>0.0):
			a = flux(um[i])
		else:
			a = 0.0

		if (up[i]>0.0):
			b = 0.0
		else:
			b = flux(up[i])

		fhat[i] = a+b

	return fhat

# Lax-Wendroff flux
def lw_flux(um, up, dx, dt):
	fm = flux(um)
	fp = flux(up)

	return 0.5*(fm+fp) - 0.5*(dt/dx)*0.5*(um+up)*(fp-fm)	

# specify domain
a = 0
b = 2*np.pi

# initial condition: u(x,0) = alpha + beta*sin(x)
alpha = 0.0
beta  = 1.0

# number of grid points in spatial discretization
N  = 80

# stopping time
T = 1.5

# setup grid points
x = np.linspace(a,b,N)     
dx = (b-a)/(N-1);  

# setup array to store cell averages; due to periodicity, we omit the last cell
u = np.zeros((len(x)-1,1)); 

# compute cell averages at t=0
for i in range(0,N-1):
    u[i] = (1.0/dx)*integrate.quad(initial_condition, x[i], x[i+1], args=(alpha,beta))[0]

# set the time step
dt = dx/(2*np.amax(np.amax(u)))

# this is the main time-stepping loop
t = 0.0
while t < T:
	# alpha for the Lax-Friedrichs flux
	A  = np.amax(np.amax(u));

	# compute numerical fluxes fhat_{j+1/2}
	um = u
	up = np.roll(u,-1)
	# fR = roe_flux(um, up)
	# fR = godunov_flux(um, up) 
	fR = lf_flux(um, up, A)
	# fR = eo_flux(um, up)
	# fR = lw_flux(um, up, dx, dt)

	# compute numerical fluxes fhat_{j-1/2} (assuming periodic BCs)
	fL = np.roll(fR,1)

    # first order explicit time-stepping
	u -= dt/dx*(fR - fL)

	# increment time step
	t = t+dt
     

# setup array to store cell averages of exact solution
Ne = 320
xe = np.linspace(a,b,Ne)
ue = np.zeros((len(xe)-1,1));
dxe = (b-a)/(Ne-1)

# compute cell averages of exact solution
for i in range(0,Ne-1):
    ue[i] = (1.0/dxe)*integrate.quad(exact_solution, xe[i], xe[i+1], args=(t,alpha,beta))[0]

# plot finite volume and exact solution
plt.figure(1)
plt.plot(x, np.append(u, u[0]), label='FV')
plt.plot(xe, np.append(ue, ue[0]), label='Exact')
plt.xlabel('x')
plt.ylabel('u(x)')
plt.xlim([0.0, 2.0*np.pi])
plt.title('Finite Volume Solution to Burgers Equation')
plt.legend()
plt.show()
