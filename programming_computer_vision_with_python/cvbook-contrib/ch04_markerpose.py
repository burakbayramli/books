from PIL import Image
from pylab import *
import os
import numpy
import pickle

import camera
import homography
import sift


def cube_points(c, wid):
  p = []
  # bottom
  p.append([c[0] - wid, c[1] - wid, c[2] - wid])
  p.append([c[0] - wid, c[1] + wid, c[2] - wid])
  p.append([c[0] + wid, c[1] + wid, c[2] - wid])
  p.append([c[0] + wid, c[1] - wid, c[2] - wid])
  p.append([c[0] - wid, c[1] - wid, c[2] - wid])

  # top
  p.append([c[0] - wid, c[1] - wid, c[2] + wid])
  p.append([c[0] - wid, c[1] + wid, c[2] + wid])
  p.append([c[0] + wid, c[1] + wid, c[2] + wid])
  p.append([c[0] + wid, c[1] - wid, c[2] + wid])
  p.append([c[0] - wid, c[1] - wid, c[2] + wid])

  # sides
  p.append([c[0] - wid, c[1] - wid, c[2] + wid])
  p.append([c[0] - wid, c[1] + wid, c[2] + wid])
  p.append([c[0] - wid, c[1] + wid, c[2] - wid])
  p.append([c[0] + wid, c[1] + wid, c[2] - wid])
  p.append([c[0] + wid, c[1] + wid, c[2] + wid])
  p.append([c[0] + wid, c[1] - wid, c[2] + wid])
  p.append([c[0] + wid, c[1] - wid, c[2] - wid])

  return numpy.array(p).T


l0, d0 = sift.read_or_compute('out_ch4pics/h_template.jpg',
                              'out_ch4pics/h_template.sift')
l1, d1 = sift.read_or_compute('out_ch4pics/h_image.jpg',
                              'out_ch4pics/h_image.sift')

#figure()
#gray()
im0 = array(Image.open('out_ch4pics/h_template.jpg'))
#sift.plot_features(im0, l0, circle=True)
#
#figure()
#gray()
im1 = array(Image.open('out_ch4pics/h_image.jpg'))
#sift.plot_features(im1, l1, circle=True)
#show()

if not os.path.exists('out_ch04_markerpose.pickle'):
  matches = sift.match(d0, d1)
  pickle.dump(matches, open('out_ch04_markerpose.pickle', 'wb'))
matches = pickle.load(open('out_ch04_markerpose.pickle', 'rb'))

#figure()
#gray()
#sift.plot_matches(im0, im1, l0, l1, matches, show_below=False)
#show()

ndx = matches.nonzero()[0]
fp = homography.make_homog(l0[ndx, :2].T)
ndx2 = [int(matches[i]) for i in ndx]
tp = homography.make_homog(l1[ndx2, :2].T)

#model = homography.RansacModel()
#H = homography.H_from_ransac(fp, tp, model)
# Not enough matches for H_from_ransac(), but all matches happen to be correct
# anyways, so no need for that.
H = homography.H_from_points(fp, tp)


K = camera.my_calibration(im0.shape[:2])

# How big this appears depends on the z translation in cam1 in the cam1 line.
box = cube_points([0, 0, 0.1], 0.1)

# project bottom square in first image
cam1 = camera.Camera(numpy.hstack((K, numpy.dot(K, array([[0], [0], [-1]])))))
box_cam1 = cam1.project(homography.make_homog(box[:, :5]))

# transfer points to second image
box_trans = homography.normalize(numpy.dot(H, box_cam1))

# compute second camera matrix
cam2 = camera.Camera(numpy.dot(H, cam1.P))

# H * P transforms points in z = 0 correctly, so its first two colums are
# correct. The upper left 3x3 submatrix should have orthogonal vectors, use
# this fact to reconstruct the third (H contains a scale, so this is only
# well-defined up to scale of the third axis. Fudge something up.)
A = numpy.dot(numpy.linalg.inv(K), cam2.P[:, :3])
lenx = numpy.sqrt(numpy.dot(A[:, 0], A[:, 0]))
leny = numpy.sqrt(numpy.dot(A[:, 1], A[:, 1]))
#print lenx, leny  # Should be similar.
zscale = (lenx + leny) / (2 * lenx * leny)
A = numpy.array([A[:, 0], A[:, 1], zscale * numpy.cross(A[:, 0], A[:, 1])]).T
cam2.P[:, :3] = numpy.dot(K, A)

# project directly with second camera
box_cam2 = cam2.project(homography.make_homog(box))

# test: projecting point on z=0 should be the same either way
point = numpy.array([[1, 1, 0, 1]]).T
print homography.normalize(numpy.dot(numpy.dot(H, cam1.P), point)).T
print homography.normalize(numpy.dot(H, cam1.project(point))).T
print cam2.project(point).T


# visualize

# 2d projection of bottom square in template
figure()
imshow(im0)
plot(box_cam1[0, :], box_cam1[1, :], linewidth=3)

# 2d projection of bottom square in image, transferred with H
#figure()
#imshow(im1)
#plot(box_trans[0, :], box_trans[1, :], linewidth=3)

# 3d cube
figure()
imshow(im1)
plot(box_cam2[0, :], box_cam2[1, :], linewidth=3)

import pickle
with open('out_ch4_camera.pickle', 'wb') as f:
  pickle.dump(K, f)
  pickle.dump(numpy.dot(numpy.linalg.inv(K), cam2.P), f)

show()
