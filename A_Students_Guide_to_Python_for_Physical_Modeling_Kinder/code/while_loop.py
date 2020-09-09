# while_loop.py
# -------------------------------------------------------------------------
# Use a while loop to generate multiple solutions to the quadratic equation.
# ------------------------------------------------------------------------- 
import numpy as np

a, b, c = 2, 2, -1
while (b**2 - 4*a*c >= 0):
	x = (-b + np.sqrt(b**2 - 4*a*c)) / (2*a)
	print("a = {:.4f}, x = {:.4f}".format(a,x))
	a = a - 0.3
print("done!")
