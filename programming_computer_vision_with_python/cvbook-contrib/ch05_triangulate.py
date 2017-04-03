from pylab import *
import sfm

execfile('load_vggdata.py')

# Points in first 2 views.
ndx = (corr[:, 0] >= 0) & (corr[:, 1] >= 0)

x1 = points2D[0][:, corr[ndx, 0]]
x1 = numpy.vstack( (x1, numpy.ones(x1.shape[1])) )
x2 = points2D[1][:, corr[ndx, 1]]
x2 = numpy.vstack( (x2, numpy.ones(x2.shape[1])) )

Xtrue = points3D[:, ndx]
Xtrue = numpy.vstack( (Xtrue, numpy.ones(Xtrue.shape[1])) )

Xest = sfm.triangulate(x1, x2, P[0].P, P[1].P)

# Check 3 points:
print Xest[:, :3]
print Xtrue[:, :3]

from mpl_toolkits.mplot3d import axes3d
fig = figure()
ax = fig.gca(projection='3d')
ax.plot(Xest[0], Xest[1], Xest[2], 'ko')
ax.plot(Xtrue[0], Xtrue[1], Xtrue[2], 'r.')
axis('equal')

show()
