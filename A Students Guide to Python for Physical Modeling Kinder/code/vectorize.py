# vectorize.py
# -------------------------------------------------------------------------
# Use vectorized operations to generate multiple solutions to the
# quadratic equation.
# ------------------------------------------------------------------------- 
import numpy as np

b, c = 2, -1
a = np.arange(-1, 2, 0.3)
x = (-b + np.sqrt(b**2 - 4*a*c)) / (2*a)
