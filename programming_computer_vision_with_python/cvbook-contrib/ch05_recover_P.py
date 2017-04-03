from pylab import *
import sfm, camera

execfile('load_vggdata.py')

corr = corr[:, 0]
ndx3D = numpy.where(corr >= 0)[0]
ndx2D = corr[ndx3D]

x = points2D[0][:, ndx2D]
x = numpy.vstack( (x, numpy.ones(x.shape[1])) )
X = points3D[:, ndx3D]
X = numpy.vstack( (X, numpy.ones(X.shape[1])) )

Pest = camera.Camera(sfm.compute_P(x,X))

# Check:
print Pest.P / Pest.P[2, 3]
print P[0].P / P[0].P[2, 3]

xest = Pest.project(X)

figure()
imshow(im1)
plot(x[0], x[1], 'bo')
plot(xest[0], xest[1], 'r.')
axis('off')

show()

