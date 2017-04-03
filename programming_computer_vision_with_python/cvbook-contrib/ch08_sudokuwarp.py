import os
from PIL import Image
from pylab import *
from scipy import ndimage

import homography
import imtools
import tic



SUDOKU_PATH = '/Users/thakis/Downloads/data/sudoku_images/sudokus/'
imname = os.path.join(SUDOKU_PATH, 'sudoku8.jpg')

im = array(Image.open(imname).convert('L'))

# Ask user for corners.
figure()
imshow(im)
gray()
x = ginput(4)

# top left, top right, bottom right, bottom left
# Note: The book switches fp from (x, y) order in |x| to (y, x) order in
# fp. imtools.Htransform() expects (x, y) order, so don't do this switching
# here.
fp = array([array([p[0], p[1], 1]) for p in x]).T
tp = array([[0, 0, 1], [1000, 0, 1], [1000, 1000, 1], [0, 1000, 1]]).T
H = homography.H_from_points(tp, fp)

tic.k('starting warp')
im_g = imtools.Htransform(im, H, (1000, 1000))
tic.k('warped')

figure()
imshow(im_g)
gray()
show()
