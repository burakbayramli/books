"""
Integrate H(x-x0) between -1 and 1 using the Trapezoidal rule
and the corresponding Gauss-Legendre rule with the same no of points (+1).
"""

# Point of discontinuity
x0 = 0.01651298
#x0 = 0

def H(x):
    return np.where(x >= x0, 1.0, 0.0)

def integral_of_H(x):
    return 0.0 if x <= x0 else (x-x0)

def cosine(x):
    return np.cos(x)

def integral_of_cosine(x):
    return np.sin(x) - np.sin(-1)

def Trapezoidal(f, a, b, n):
    """Trapezoidal integration on [a,b] with n intervals."""
    h = (b-a)/float(n)
    x = np.linspace(a, b, n+1)
    y = f(x)
    y[0] /= 2
    y[-1] /= 2
    I = h*np.sum(y)
    return I

def GaussLegendre(f, n):
    """Gauss-Legendre integration on [-1, 1] with n points."""
    x, w = numint.GaussLegendre(n)
    I = np.dot(f(x), w)
    return I

import numint, numpy as np

def experiment(f, f_integrated):
    x_points = list(range(1, 21)) + [32, 64, 96, 100, 128, 256, 512, 1024]
    GL_errors = []
    Tz_errors = []
    for n in x_points:
        I_Tz = Trapezoidal(f, -1, 1, n)
        I_GL = GaussLegendre(f, n)
        I_ex = f_integrated(1)
        GL_errors.append(abs(I_ex - I_GL))
        Tz_errors.append(abs(I_ex - I_Tz))
        print('%4d %12.4E %12.4E' % (n, I_ex - I_Tz, I_ex - I_GL))

experiment(H, integral_of_H)
print('\n---- and now a smooth function ----')
experiment(cosine, integral_of_cosine)


