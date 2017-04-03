from PIL import Image
import glob
import numpy
import os
import cPickle as pickle

import camera
import homography
import sfm
import sift
import tic

imname = glob.glob('out_corner/IMG_*.jpg')
#imname = glob.glob('out_alcatraz/*.jpg')
siftname = [os.path.splitext(im)[0] + '.sift' for im in imname]

tic.k('start')

# For out_corner, this increases feature count from 4k to 16k and matches from
# 100 to 170 (which helps quality, but also slows down the program a lot, from
# from 20s to 60s):
# (with twosided matching, matches go from 85 to 113 for out_corner)
# NOTE: delete caches after changing this!
histeq = False

l, d = {}, {}
for i in range(len(imname)):
  l[i], d[i] = sift.read_or_compute(imname[i], siftname[i], histeq)

tic.k('loaded sifts')

print '{} / {} features'.format(len(d[0]), len(d[1]))
if not os.path.exists('out_ch05_recover_match.pickle'):
  #matches = sift.match(d[0], d[1])
  matches = sift.match_twosided(d[0], d[1])
  pickle.dump(matches, open('out_ch05_recover_match.pickle', 'wb'))
matches = pickle.load(open('out_ch05_recover_match.pickle', 'rb'))

tic.k('matched')

ndx = matches.nonzero()[0]
x1 = homography.make_homog(l[0][ndx, :2].T)
ndx2 = [int(matches[i]) for i in ndx]
x2 = homography.make_homog(l[1][ndx2, :2].T)

print '{} matches'.format(len(ndx))

image = [numpy.array(Image.open(name)) for name in imname]

# calibration (FIXME?)
K = camera.my_calibration(image[0].shape[:2])

# Normalize with inv(K) (allows metric reconstruction).
x1n = numpy.dot(numpy.linalg.inv(K), x1)
x2n = numpy.dot(numpy.linalg.inv(K), x2)

tic.k('normalized')

# Estimate E.
model = sfm.RansacModel()
# Note that x2n is passed as first parameter, since F_from_ransac() and friends
# compute the F matrix mapping from the 2nd parameter to the first, and the
# code below gives camera 1 the identity transform.
E, inliers = sfm.F_from_ransac(x2n, x1n, model)

tic.k('ransacd')

# compute camera matrices

P1 = numpy.array([[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 1, 0]])
P2 = sfm.compute_P_from_essential(E)

tic.k('computed possible camera matrices')

# Pick the solution with points in front of cameras
ind = 0
maxres = 0
for i in range(4):
  X = sfm.triangulate(x1n[:, inliers], x2n[:, inliers], P1, P2[i])
  d1 = numpy.dot(P1, X)[2]
  d2 = numpy.dot(P2[i], X)[2]
  res = numpy.sum(d1 > 0) + numpy.sum(d2 > 0)
  if res > maxres:
    maxres = res
    ind = i
    infront = (d1 > 0) & (d2 > 0)

tic.k('picked one')

X = sfm.triangulate(x1n[:, inliers], x2n[:, inliers], P1, P2[ind])
X = X[:, infront]

tic.k('triangulated')


# Plot!
from mpl_toolkits.mplot3d import axes3d
from pylab import *

fig = figure()
ax = fig.gca(projection='3d')
ax.plot(X[0], X[1], X[2], 'k.')
axis('off')

cam1 = camera.Camera(P1)
cam2 = camera.Camera(P2[ind])
x1p = cam1.project(X)
x2p = cam2.project(X)

x1p = numpy.dot(K, x1p)
x2p = numpy.dot(K, x2p)

figure()
imshow(image[0])
gray()
plot(x1p[0], x1p[1], 'o')
plot(x1[0], x1[1], 'r.')
axis('off')

figure()
imshow(image[1])
gray()
plot(x2p[0], x2p[1], 'o')
plot(x2[0], x2[1], 'r.')
axis('off')

#figure()
#sift.plot_matches(image[0], image[1], l[0], l[1], matches, show_below=True)

show()
