import camera
import numpy

K = numpy.array([[1000, 0, 500],
                 [0, 1000, 300],
                 [0, 0, 1]])
tmp = camera.rotation_matrix([0, 0, 1.0])[:3, :3]
Rt = numpy.hstack((tmp, numpy.array([[50], [40], [30]])))
cam = camera.Camera(numpy.dot(K, Rt))

print K, Rt
print cam.factor()
print cam.center()
