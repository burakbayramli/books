import unittest

from pylab import *
import numpy

import camera
import homography
import sfm

class SfmTest(unittest.TestCase):
  def setUp(self):
    points = array([
      [-1.1, -1.1, -1.1], [ 1.4, -1.4, -1.4], [-1.5,  1.5, -1], [ 1,  1.8, -1],
      [-1.2, -1.2,  1.2], [ 1.3, -1.3,  1.3], [-1.6,  1.6,  1], [ 1,  1.7,  1],
      ])
    points = homography.make_homog(points.T)

    P = hstack((eye(3), array([[0], [0], [0]])))
    cam = camera.Camera(P)
    self.x = cam.project(points)

    r = [0.05, 0.1, 0.15]
    rot = camera.rotation_matrix(r)
    cam.P = dot(cam.P, rot)
    cam.P[:, 3] = array([1, 0, 0])
    self.x2 = cam.project(points)

    K, R, t = cam.factor()
    self.expectedE = dot(sfm.skew(t), R)
    self.expectedE /= self.expectedE[2, 2]

  def testComputeFundamental(self):
    E = sfm.compute_fundamental(self.x2[:, :8], self.x[:, :8])
    self.assertEqual(self.expectedE.shape, E.shape)
    self.assertTrue(numpy.allclose(self.expectedE, E))

  def testComputeFundamentalNormalized(self):
    E = sfm.compute_fundamental_normalized(self.x2[:, :8], self.x[:, :8])
    self.assertEqual(self.expectedE.shape, E.shape)
    self.assertTrue(numpy.allclose(self.expectedE, E))


if __name__ == '__main__':
  unittest.main()
