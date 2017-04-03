from PIL import Image
from pylab import *
import cPickle as pickle
import glob
import os
import numpy

import homography
import sift
import tic
import warp

imname = glob.glob('out_corner/IMG_*.jpg')
siftname = [os.path.splitext(im)[0] + '.sift' for im in imname]

tic.k('start')

l, d = {}, {}
for i in range(len(imname)):
  l[i], d[i] = sift.read_or_compute(imname[i], siftname[i])

tic.k('loaded sifts')


if not os.path.exists('out_ch03_ex06_match.pickle'):
  matches = sift.match(d[1], d[0])
  pickle.dump(matches, open('out_ch03_ex06_match.pickle', 'wb'))
matches = pickle.load(open('out_ch03_ex06_match.pickle', 'rb'))

tic.k('matched')

ndx = matches.nonzero()[0]
fp = homography.make_homog(l[1][ndx, :2].T)
ndx2 = [int(matches[i]) for i in ndx]
tp = homography.make_homog(l[0][ndx2, :2].T)


tic.k('converted')

im1 = array(Image.open(imname[0]).convert('L'))
im2 = array(Image.open(imname[1]).convert('L'))
if len(im1.shape) == 2:
  gray()
imshow(im1)

tic.k('imloaded')

model = homography.RansacModel()

colors = ['rx', 'gx', 'bx']
i = 0
# The ransac in H_from_ransac needs 4 model points and at least 10 additional
# inliers.
while fp.shape[1] >= 14:
  try:
    H, inliers = homography.H_from_ransac(fp, tp, model)
  except:
    break
  print 'inlier set size', len(inliers)

  inliers_pts = tp[:2, inliers]
  plot(inliers_pts[0], inliers_pts[1], colors[i])

  # Compute complement.
  fp = numpy.delete(fp, inliers, axis=1)
  tp = numpy.delete(tp, inliers, axis=1)
  i += 1

# Plot remaining matches.
print 'remaining set size', fp.shape[1]
plot(tp[0], tp[1], 'yx')

tic.k('sets computed')

show()
