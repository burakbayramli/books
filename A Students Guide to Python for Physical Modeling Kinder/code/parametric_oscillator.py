# parametric_oscillator.py
# -------------------------------------------------------------------------
# Define a parametric function that accepts 4 parameters then integrate it
# using odeint.
# ------------------------------------------------------------------------- 
import numpy as np
from scipy.integrate import odeint

def F(y, t, spring_constant=1.0, mass=1.0):
	"""
	Return derivatives for harmonic oscillator:
		y'' = -(k/m) * y
	y = displacement in [m]
	k = spring_constant in [N/m]
	m = mass in [kg]
	"""
	dy = [0, 0]			# array to store derivatives
	dy[0] = y[1]		
	dy[1] = -(spring_constant/mass) * y[0]	
	return dy

# -------------------------------------------------------------------------
# Integrate parametric function using two different methods.
# ------------------------------------------------------------------------- 
y0 = (1.0, 0.0)						# initial conditions
t = np.linspace(0, 10, 101)			# times at which y(t) will be evaluated

# Method 1 -- dummy function
def G(y, t): return F(y, t, 2.0, 0.5)
yA = odeint(G, y0, t)

# Method 2 -- keywords
yB = odeint(F, y0, t, args=(2.0, 0.5))
