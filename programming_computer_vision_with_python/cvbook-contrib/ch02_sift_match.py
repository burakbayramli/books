from PIL import Image
from pylab import *
import sys

import sift
import imtools

if len(sys.argv) >= 3:
  im1f, im2f = sys.argv[1], sys.argv[2]
else:
  im1f = '/Users/thakis/src/PCV/data/sf_view1.jpg'
  im2f = '/Users/thakis/src/PCV/data/sf_view2.jpg'
im1 = array(Image.open(im1f).convert('L'))
im2 = array(Image.open(im2f).convert('L'))

sift.process_image(im1f, 'out_sift_1.txt')
l1, d1 = sift.read_features_from_file('out_sift_1.txt')
figure(); gray(); sift.plot_features(im1, l1, circle=True)

sift.process_image(im2f, 'out_sift_2.txt')
l2, d2 = sift.read_features_from_file('out_sift_2.txt')
figure(); gray(); sift.plot_features(im2, l2, circle=True)

#matches = sift.match(d1, d2)
matches = sift.match_twosided(d1, d2)
print '{} matches'.format(len(matches.nonzero()[0]))

figure()
gray()
sift.plot_matches(im1, im2, l1, l2, matches, show_below=False)
show()
