# multiprocessing_example.py
import multiprocessing as mp
import matplotlib.pyplot as plt
import numpy as np
from supf import supf_wrapper
if __name__ == '__main__':
    reps = 1000
    T = 200
    setup = []
    for i in xrange(reps):
        y = np.random.standard_normal(T)
        x = np.random.standard_normal((T, 1))
        p = 0.2
        setup.append((y, x, p))
        
    # Non parallel map
    # res = map(surf_wrapper, setup)
    # Parallel map
    po = mp.Pool(processes=2)
    res = po.map(supf_wrapper, setup)
    print(len(res))
    po.close()
    ax = plt.hist(res)
    ax = ax[2]
    fig = ax[0].get_figure()
    fig.savefig('/tmp/multiprocessing.pdf')
    
