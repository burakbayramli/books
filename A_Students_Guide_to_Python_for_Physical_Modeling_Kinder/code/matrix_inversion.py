# matrix_inversion.py
# -------------------------------------------------------------------------
# Invert a simple matrix to solve a system of linear equations.
# ------------------------------------------------------------------------- 
import numpy as np
from scipy.linalg import inv

#%% Set up and solve C.x = a
a = np.array([-1, 5])
C = np.array([[1, 3], [3, 4]])
x = np.dot(inv(C), a)

#%% Check solution.
error = np.dot(C,x) - a
