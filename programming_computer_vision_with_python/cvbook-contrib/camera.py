import numpy
import scipy.linalg

class Camera(object):
  '''Represents a pin-hole camera.'''

  def __init__(self, P):
    self.P = P
    self.K = None  # Calibration matrix.
    self.R = None  # Rotation matrix.
    self.t = None  # Translation vector.
    self.c = None  # Camera center.

  def project(self, X):
    x = numpy.dot(self.P, X)
    for i in range(3):
      x[i] /= x[2]
    return x

  def factor(self):
    '''Factor camera matrix P into K, R, t such that P = K[R|t].'''

    # Factor 3x3 part.
    K, R = scipy.linalg.rq(self.P[:, :3])

    # Make sure K has a positive determinant.
    T = numpy.diag(numpy.sign(numpy.diag(K)))
    if numpy.linalg.det(T) < 0:
      T[1, 1] *= -1

    self.K = numpy.dot(K, T)
    self.R = numpy.dot(T, R)  # T is self-inverse
    self.t = numpy.dot(numpy.linalg.inv(self.K), self.P[:, 3])

    return self.K, self.R, self.t

  def center(self):
    '''Returns the camera center, the point in space projected to (0, 0) on
    screen.'''
    if self.c is None:
      self.factor()
      self.c = -numpy.dot(self.R.T, self.t)
    return self.c


def rotation_matrix(a):
  '''Returns a rotation matrix around the axis of a, by an angle that's equal
  to the length of a in radians.'''
  R = numpy.eye(4)
  R[:3, :3] = scipy.linalg.expm([[0, -a[2], a[1]],
                                 [a[2], 0, -a[0]],
                                 [-a[1], a[0], 0]])
  return R


def my_calibration(sz):
  dX = 10.0 # cm
  dY = 8.0 # cm
  dZ = 30.0 # cm

  # from Acorn:
  dx = 1744 - 974 # px
  dy = 1269 - 650 # px

  fx = (dx / dX) * dZ
  fy = (dy / dY) * dZ

  row, col = sz
  fx = fx * col / 2592
  fy = fy * row / 1944
  K = numpy.diag([fx, fy, 1])
  K[0, 2] = 0.5 * col
  K[1, 2] = 0.5 * row
  return K
