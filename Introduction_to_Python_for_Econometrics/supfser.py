import numpy as np
from numpy.linalg import pinv
def supf(y, x, p):
    T = y.shape[0]
    range = np.floor(np.array([T * p, T * (1 - p)]))
    range = np.arange(range[0], range[1] + 1, dtype=np.int32)
    x = x - np.mean(x)
    y = y - np.mean(y)
    b = pinv(x).dot(y)
    e = y - x.dot(b)
    R2_r = 1 - e.dot(e) / y.dot(y)
    k = x.shape[1]
    F_stat = np.zeros(T)
    for t in range:
        X1 = x[:t]
        X2 = x[t:]
        b = pinv(X1).dot(y[:t])
        e[:t] = y[:t] - X1.dot(b)
        b = pinv(X2).dot(y[t:])
        e[t:] = y[t:] - X2.dot(b)
        R2_u = 1 - e.dot(e) / y.dot(y)
        F_stat[t] = ((R2_u - R2_r) / k) / ((1 - R2_u) / (T - k))
    print F_stat.argmax()
    return F_stat.max()
    
reps = 100
T = 200
k = 1
p = 0.2
sup_F_stat = np.zeros(reps)
for j in xrange(reps):
    y = np.random.standard_normal(T)
    x = np.random.standard_normal((T,k))
    sup_F_stat[j] = supf(y, x, p)
