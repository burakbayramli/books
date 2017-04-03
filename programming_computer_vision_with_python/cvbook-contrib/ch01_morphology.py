from PIL import Image
from pylab import *
from scipy.ndimage import measurements, morphology
from scipy import misc
import numpy

#im = array(Image.open('board.jpeg').convert('L'))
im = misc.lena()
im = 1 * (im < 128)
gray()

subplot(2, 3, 1)
title('original')
imshow(im)

subplot(2, 3, 2)
im_close = morphology.binary_closing(im, ones((5, 5)), iterations=2)
title('closing')
imshow(im_close)

subplot(2, 3, 5)
labels_close, obj_count_close = measurements.label(im_close)
# FIXME: This seems to miss the left half of the face?
imshow(labels_close)
print obj_count_close, 'object on closing image'

subplot(2, 3, 3)
im_open = morphology.binary_opening(im, ones((5, 5)), iterations=2)
title('opening')
imshow(im_open)

subplot(2, 3, 6)
labels_open, obj_count_open = measurements.label(im_open)
imshow(labels_open)
print obj_count_open, 'object on opening image'

#misc.imsave('out_labels.jpg', labels_open)

show()
