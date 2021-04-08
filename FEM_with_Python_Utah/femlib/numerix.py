import sys
import numpy as np
from math import sqrt

def areclose(a, b, rtol=1.e-10, ftol=1.e-8, tol=None):
    if tol is not None:
        rtol = ftol = tol
    a = np.asarray(a).flatten()
    b = np.asarray(b).flatten()
    dnom = norm(a)
    absdiff = abs(np.amax(a - b))
    return absdiff / dnom < rtol and absdiff < ftol

def allclose(a, b):
    return np.allclose(a, b)

def aslist(a, dtype=None, like=None):
    if like is not None:
        a = [a for i in range(len(like))]
    try: len(a)
    except TypeError: a = [a]
    if dtype is None:
        return [x for x in a]
    return [dtype(x) for x in a]

def asstring(a, disp=1):
    a = np.asarray(a)
    string = ' '.join('{}'.format(x) for x in a.flatten())
    if disp:
        return 0 if len(a.shape) == 1 else a.shape[1], string
    return string

def asarray(string, offsets=None, dtype=float):
    a = np.array(string.split(), dtype=dtype)
    if offsets: return a.reshape(-1, offsets)
    return a

def norm(a):
    a = np.asarray(a)
    return sqrt(np.dot(a, a))

def relerr(a, b):
    a = np.asarray(a)
    b = np.asarray(b)
    return norm(a - b) / norm(a)

def midpoint(a):
    M = np.amax(a)
    m = np.amin(a)
    return (m + M) / 2.
