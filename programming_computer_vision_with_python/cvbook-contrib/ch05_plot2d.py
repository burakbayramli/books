from pylab import *

execfile('load_vggdata.py')

# Show 2d points.
figure()
imshow(im1)
plot(points2D[0][0], points2D[0][1], '*')
axis('off')

# Show projected 3d points.
X = numpy.vstack( (points3D, numpy.ones(points3D.shape[1])) )
x = P[0].project(X)

figure()
imshow(im1)
plot(x[0], x[1], 'r.')
axis('off')

show()
