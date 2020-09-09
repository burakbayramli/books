# quadrature.py
# -------------------------------------------------------------------------
# Integrate two functions using quad.
# ------------------------------------------------------------------------- 
import numpy as np
from scipy.integrate import quad

#%% Integrate a built-in function.
upper_limit = np.linspace(0,3*np.pi,16)
cos_integral = np.zeros(upper_limit.size)
for i in range(upper_limit.size):
    cos_integral[i], error = quad(np.cos, 0, upper_limit[i])

#%% Now integrate a user-defined function.
def integrand(x): return np.exp(-x**2/2)

upper_limit = np.linspace(0, 5, 51)
gauss_integral = np.zeros(upper_limit.size)
for i in range(upper_limit.size):
    gauss_integral[i], error = quad(integrand, 0, upper_limit[i])
