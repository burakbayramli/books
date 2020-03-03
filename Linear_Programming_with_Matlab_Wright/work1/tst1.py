from scipy.optimize import linprog
import numpy as np

A = np.array([[1,  1, 1, 0],
              [1,  3, 0, 1]])

b = np.array([5,7])

c = np.array([-1, -5, 0, 0 ])
print (A.shape)
print (c.shape)

res = linprog(c, A_eq=A, b_eq=b, options={"disp": True})

print (res)

#  [1.15454732e-13, 2.33333333e+00, 2.66666667e+00, 3.96953400e-12]

