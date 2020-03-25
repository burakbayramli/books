""" nearestnb.py """

import numpy as np
from numpy.random import rand,randn 
import matplotlib.pyplot as plt
from scipy.spatial import Voronoi, voronoi_plot_2d

np.random.seed(12345)
M = 80 
x = randn(M,2)
y = np.zeros(M) # pre-allocate list

for i in range(M): 
    if rand()<0.5: 
        x[i,1], y[i] = x[i,0] + np.abs(randn()), 0
    else: 
        x[i,1], y[i] = x[i,0] - np.abs(randn()), 1

vor = Voronoi(x)   
plt_options = {'show_vertices':False, 'show_points':False, 'line_alpha':0.5}
fig = voronoi_plot_2d(vor, **plt_options)
plt.plot(x[y==0,0], x[y==0,1],'bo', 
         x[y==1,0], x[y==1,1],'rs', markersize=3)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plt.tight_layout()
fig.set_size_inches(5,4)
plt.savefig('nearestnbpy.pdf',format='pdf')
plt.show()
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

