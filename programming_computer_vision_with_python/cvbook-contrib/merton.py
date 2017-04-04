from PIL import Image
import numpy
import glob
import camera

dir = '/home/burak/Documents/Dropbox/Public/data/pcv_data/merton1'

im1 = numpy.array(Image.open(dir + '/images/001.jpg'))
im2 = numpy.array(Image.open(dir + '/images/002.jpg'))

points2D = [numpy.loadtxt(f).T for f in glob.glob(dir + '/2D/*.corners')]
points3D = numpy.loadtxt(dir + '/3D/p3d').T

corr = numpy.genfromtxt(dir + '/2D/nview-corners', dtype='int', missing_values='*')
P = [camera.Camera(numpy.loadtxt(f)) for f in glob.glob('out_merton/2D/*.P')]

from pylab import *
import sfm

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
