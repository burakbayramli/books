# for_loop.py
# -------------------------------------------------------------------------
# Use a for loop to generate multiple solutions to the quadratic equation.
# ------------------------------------------------------------------------- 
import numpy as np

b, c = 2, -1
for a in np.arange(-1, 2, 0.3):
	x = (-b + np.sqrt(b**2 - 4*a*c)) / (2*a)
	print("a= {:.4f}, x= {:.4f}".format(a,x))
