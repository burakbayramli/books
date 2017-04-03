from mpl_toolkits.mplot3d import axes3d
from pylab import *

fig = figure()
ax = fig.gca(projection="3d")

# Synthetic test data:
#X, Y, Z = axes3d.get_test_data(0.25)
#ax.plot(X.flatten(), Y.flatten(), Z.flatten(), 'o')

execfile('load_vggdata.py')
ax.plot(points3D[0], points3D[1], points3D[2], 'k.')

show()
