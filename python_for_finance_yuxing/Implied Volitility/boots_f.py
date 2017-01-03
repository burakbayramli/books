import numpy as np
def boots_f(data,n_obs,replacement= None):
    n=len(data)
    if (n<n_obs):
        print "n is less than n_abs"
    else:
        if replacement==None:
            y.np.random.permutation(data)
            return y[0:n_obs]
        else:
            y=[]
    for i in range(n_obs):
        k = np.random.permutation(data)
        y.append(k[0])
    return y
