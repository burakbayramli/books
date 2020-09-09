# simple_oscillator.py
# -------------------------------------------------------------------------
# Define function to use in solution of differential equation for a simple
# harmonic oscillator.
# ------------------------------------------------------------------------- 
def F(y, t):
	"""
	Return derivatives for 2nd order ODE y'' = -y.
	"""
	dy = [0, 0]			# preallocate list to store derivatives
	dy[0] = y[1]		# first derivative of y(t)
	dy[1] = -y[0]		# second derivative of y(t)
	return dy
