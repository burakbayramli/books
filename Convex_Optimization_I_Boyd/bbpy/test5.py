from scipy.interpolate import Rbf
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
from matplotlib import cm
import numpy as np
import itertools
import numpy.linalg as lin
import scipy.linalg as slin

from scipy.optimize import (BFGS,
                            Bounds,
                            minimize)

def rosenbrock(x):
    return (1 - x[0])**2 + 100*(x[1] - x[0]**2)**2

x0 = [-1.0,0]

opts = {'maxiter': 1000, 'verbose': 2}
res = minimize (fun=rosenbrock,
                x0=x0,
                method = 'trust-constr',
                jac = "2-point",
                hess = BFGS (),
                bounds=Bounds([0.0, 0.5], [0.1, 2.0]),
#                bounds=Bounds([0.0, 0.0], [3.0, 3.0]),
                options=opts)

print (res)
