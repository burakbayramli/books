from PIL import Image
from pylab import *
import sys

import homography
import imtools

if len(sys.argv) != 2:
  print 'usage: %prog image.jpeg'
  sys.exit(1)

imname = sys.argv[1]
im = array(Image.open(imname))
imshow(im)
corners = array(ginput(4))
# FIXME: sort corners, currently expects tl, bl, br, tr order.

# Fake more realistic side lengths.
h = 400
w = int(np.linalg.norm(corners[3] - corners[0]) /
        np.linalg.norm(corners[1] - corners[0]) * h)

corners = homography.make_homog(corners.T)
normalized = homography.make_homog(array([[0, 0, w, w], [0, h, h, 0]]))
H = homography.H_from_points(normalized, corners)

#normalized = homography.make_homog(array([[0, 0, 1, 1], [0, 1, 1, 0]]))
#print homography.H_from_points(normalized, corners)
#print H

if True:
  overshoot = 0.8  # Adds a 80% border around the selected area.
  s = (2 * overshoot + 1)
  H = dot(H, array([[s, 0, -w*overshoot],
                    [0, s, -h*overshoot],
                    [0, 0, 1]]))

mapped = imtools.Htransform(im, H, (h, w))
figure()
imshow(mapped)

show()

