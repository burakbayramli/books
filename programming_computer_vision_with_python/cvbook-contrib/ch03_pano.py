from PIL import Image
from pylab import *
import cPickle as pickle
import glob
import os

import homography
import sift
import tic
import warp


imname = glob.glob('out_Photos/IMG_*.jpg')
siftname = [os.path.splitext(im)[0] + '.sift' for im in imname]

tic.k('start')

l, d = {}, {}
for i in range(len(imname)):
  l[i], d[i] = sift.read_or_compute(imname[i], siftname[i])


tic.k('loaded')

matches = {}
if not os.path.exists('out_ch03_pano.pickle'):
  for i in range(len(imname) - 1):
    matches[i] = sift.match(d[i + 1], d[i])
    # Slightly better matches, but ransac can handle the worse quality:
    #matches[i] = sift.match_twosided(d[i + 1], d[i])
  pickle.dump(matches, open('out_ch03_pano.pickle', 'wb'))
matches = pickle.load(open('out_ch03_pano.pickle', 'rb'))

tic.k('matched')

def convert_points(j):
  ndx = matches[j].nonzero()[0]
  fp = homography.make_homog(l[j + 1][ndx, :2].T)
  ndx2 = [int(matches[j][i]) for i in ndx]
  tp = homography.make_homog(l[j][ndx2, :2].T)
  return fp, tp

model = homography.RansacModel()

fp, tp = convert_points(1)
H_12 = homography.H_from_ransac(fp, tp, model)[0]
tic.k('12 homogd')

fp, tp = convert_points(0)
H_01 = homography.H_from_ransac(fp, tp, model)[0]
tic.k('01 homogd')

tp, fp = convert_points(2)  # Note: Reversed.
H_32 = homography.H_from_ransac(fp, tp, model)[0]
tic.k('32 homogd')

tp, fp = convert_points(3)  # Note: Reversed.
H_43 = homography.H_from_ransac(fp, tp, model)[0]
tic.k('43 homogd')

# FIXME: Consider using bundle adjustment and Levenberg-Marquardt instead of
# just concatenating homographies which accumulates errors.
delta = 600
H_delta2 = array([[1, 0, -2*delta], [0, 1, 0], [0, 0, 1]])

im1 = array(Image.open(imname[1]))
im2 = array(Image.open(imname[2]))
im_12 = warp.panorama(H_12, im1, im2, delta, delta)
tic.k('12 warpd')

im0 = array(Image.open(imname[0]))
im_02 = warp.panorama(dot(H_12, H_01), im0, im_12, delta, 2*delta)
tic.k('02 warpd')

im3 = array(Image.open(imname[3]))
# There are two images added on the left already, hence the H_delta2.
im_03 = warp.panorama(dot(H_32, H_delta2), im3, im_02, delta, 0)
tic.k('03 warpd')

im4 = array(Image.open(imname[4]))
im_04 = warp.panorama(dot(dot(H_32, H_43), H_delta2), im4, im_03, delta, 0)
tic.k('04 warpd')

if len(im1.shape) == 2:
  gray()
imshow(array(im_04, "uint8"))

show()
