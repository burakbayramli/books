from PIL import Image
from pylab import *
from scipy.ndimage import filters
import numpy

im = array(Image.open('board.jpeg'))

subplot(2, 2, 1)
imshow(im)

for bi, blur in enumerate([5, 10, 20]):
  im2 = zeros(im.shape)
  for i in range(3):
    im2[:,:,i] = filters.gaussian_filter(im[:,:,i], blur)
  im2 = uint8(im2)
  subplot(2, 2, 2 + bi)
  imshow(im2)

show()
