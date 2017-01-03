# supf.py
import numpy as np
from numpy.linalg import pinv
def supf(y, x, p):
    T = y.shape[0]
    range = np.floor(np.array([T * p, T * (1 - p)]))
    range = np.arange(range[0], range[1] + 1, dtype=np.int32)
    # Demean since intercept doesnt break
    x = x - np.mean(x)
    y = y - np.mean(y)
    b = pinv(x).dot(y)
    e = y - x.dot(b)
    # Compute full sample R2
    R2_r = 1 - e.dot(e) / y.dot(y)
    k = x.shape[1]
    F_stat = np.zeros(T)
    for t in range:
        X1 = x[:t]
        X2 = x[t:]
        # Parameters and errors before the break
        b = pinv(X1).dot(y[:t])
        e[:t] = y[:t] - X1.dot(b)
        # Parameters and errors after the break
        b = pinv(X2).dot(y[t:])
        e[t:] = y[t:] - X2.dot(b)
        # R2 from model with break
        R2_u = 1 - e.dot(e) / y.dot(y)
        # F stat for break at t
        F_stat[t] = ((R2_u - R2_r) / k) / ((1 - R2_u) / (T - 2* k - 1))
    # Only return maximum F stat
    return F_stat.max()
        
def supf_wrapper(args):
    # Convenience wrapper for use with map
    return supf(args[0], args[1], args[2])
    
