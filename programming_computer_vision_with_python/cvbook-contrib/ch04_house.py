from pylab import *
import numpy

import camera
import homography

points = homography.make_homog(loadtxt('house.p3d').T)

P = hstack((eye(3), array([[0], [0], [-10]])))
cam = camera.Camera(P)
x = cam.project(points)

figure()
plot(x[0], x[1], 'k.')


r = 0.05 * numpy.random.rand(3)
rot = camera.rotation_matrix(r)

figure()
for t in range(20):
  cam.P = dot(cam.P, rot)
  x = cam.project(points)
  plot(x[0], x[1], 'k.')

show()
