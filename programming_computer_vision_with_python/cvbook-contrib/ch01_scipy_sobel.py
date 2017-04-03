from PIL import Image
from pylab import *
from scipy.ndimage import filters
import numpy

im = array(Image.open('board.jpeg').convert('L'))
gray()

subplot(2, 2, 1)
imshow(im)

imx = zeros(im.shape)
filters.sobel(im, 1, imx)
subplot(2, 2, 2)
imshow(imx)

imy = zeros(im.shape)
filters.sobel(im, 0, imy)
subplot(2, 2, 3)
imshow(imy)

mag = numpy.sqrt(imx**2 + imy**2)
subplot(2, 2, 4)
imshow(mag)

show()
