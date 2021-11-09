#!/usr/bin/env python

"""

an example of solving Laplace's equation via smoothing only.  Here, we
seed a multi-mode solution as the initial guess

"""

import numpy as np
import matplotlib.pyplot as plt
import smooth


def f0(x):
    # the righthand side
    return np.zeros_like(x)


#---------------------------------------------------------------------------
# look at the convergence of the m = 1 guess
N = 128

modes = [[1], [8], [16], [1, 8, 16]]
iters = [1, 10, 100, 1000]

for m in modes:
    plt.clf()

    c = ["c", "b", "g", "r"]

    for nsmooth in iters:
        a = smooth.smooth_run(N, nsmooth=nsmooth, modes=m, return_sol=True, rhs=f0)
        color = c.pop()
        plt.plot(a.x[a.ilo:a.ihi+1], a.get_solution()[a.ilo:a.ihi+1],
                 color=color, label = "# smooth = {}".format(nsmooth))

    plt.xlabel(r"$x$", fontsize="large")
    plt.ylabel("absolute error")
    plt.legend(frameon=False, fontsize="medium", loc="best")

    ax = plt.gca()
    ax.spines['left'].set_position('zero')                                                     
    ax.spines['right'].set_color('none')                                                       
    ax.spines['bottom'].set_position('zero')                                                   
    ax.spines['top'].set_color('none')                                                         
    ax.spines['left'].set_smart_bounds(True)                                                   
    ax.spines['bottom'].set_smart_bounds(True)                                                 
    ax.xaxis.set_ticks_position('bottom')                                                      
    ax.yaxis.set_ticks_position('left')             

    f = plt.gcf()
    f.set_size_inches(6.0, 6.0)

    if len(m) == 1:
        plt.savefig("smooth-mode{}.pdf".format(m[0]))
    else:
        plt.savefig("smooth-multimode.pdf")
