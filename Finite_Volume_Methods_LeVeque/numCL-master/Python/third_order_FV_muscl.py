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

# third-order polynomial reconstruction
def polynomial_reconstruction(u):

	# compute u_{j+1/2}^{-} and u_{j+1/2}^{+}
	um = -1.0/6.0*np.roll(u,1) + 5.0/6.0*u + 1.0/3.0*np.roll(u,-1)
	up = 1.0/3.0*u + 5.0/6.0*np.roll(u,-1) - 1.0/6.0*np.roll(u,-2)

	return um, up

# generalized MUSCL limiter
def muscl_limiter(um, up, u):

	# gradients to be adjusted
	ut  = um - u
	utt = u - np.roll(up,1)

	ut_m = minmod(ut, np.roll(u,-1)-u, u-np.roll(u,1))
	utt_m = minmod(utt, np.roll(u,-1)-u, u-np.roll(u,1))

	# modify the cell reconstructions using ut and utt
	um_mod = u + ut_m
	up_mod = u - utt_m
	up_mod = np.roll(up_mod,-1)

	return um_mod, up_mod

# the minmod function defined by
#	minmod(a1,...,an) = sgn(a1)*min_{j} |a_{j}|, if sgn(a1)=...=sgn(an)
#						0, 						 otherwise
#
def minmod(a, b, c):
	
	# check whether a, b, and c are the same sign
	signs = ((np.sign(a)==np.sign(b)) & (np.sign(b)==np.sign(c)) & (np.sign(c)==np.sign(a)))

	# compute minimum magnitudes of {a,b,c}
	vals = np.concatenate((a,b,c), axis=1)
	mins = np.amin(np.abs(vals), axis=1)

	# compute the minmod
	m = signs*np.sign(a)*np.reshape(mins,(len(vals),1))

	return m

# specify domain
a = 0
b = 2*np.pi

# initial condition: u(x,0) = alpha + beta*sin(x)
alpha = 0.0
beta  = 1.0

# number of grid points in spatial discretization
N  = 160

# stopping time
T = 1.5

# setup grid points
x = np.linspace(a,b,N)     
dx = (b-a)/(N-1);  

# setup array to store cell averages; due to periodicity, we omit the last cell
u0 = np.zeros((len(x)-1,1)); 

# compute cell averages at t=0
for i in range(0,N-1):
    u0[i] = (1.0/dx)*integrate.quad(initial_condition, x[i], x[i+1], args=(alpha,beta))[0]

# set the time step
dt = dx/(2*np.amax(np.amax(u0)))

# this is the main time-stepping loop
t = 0.0
while t < T:
	# alpha for the Lax-Friedrichs flux
	A  = np.amax(np.amax(u0))

	# first RK stage
	um,up = polynomial_reconstruction(u0)
	um,up = muscl_limiter(um,up,u0)
	# fR = godunov_flux(um,up)
	fR = lf_flux(um,up,A)
	fL = np.roll(fR,1)
	u = u0 - dt/dx*(fR - fL)

	# second RK stage
	um,up = polynomial_reconstruction(u)
	um,up = muscl_limiter(um,up,u)
	# fR = godunov_flux(um,up)
	fR = lf_flux(um,up,A)  
	fL = np.roll(fR,1)
	u = 3.0/4.0*u0 + 1.0/4.0*(u - dt/dx*(fR - fL))

	# third RK stage
	um,up = polynomial_reconstruction(u)
	um,up = muscl_limiter(um,up,u)
	# fR = godunov_flux(um,up)
	fR = lf_flux(um,up,A)   
	fL = np.roll(fR,1)
	u = 1.0/3.0*u0 + 2.0/3.0*(u - dt/dx*(fR - fL))

	# increment time step
	u0 = u
	t = t+dt
     

# setup array to store cell averages of exact solution
Ne = 500
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
plt.title('FV3 MUSCL Solution to Burgers Equation')
plt.legend()
plt.show()
