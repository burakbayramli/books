from PIL import Image
from pylab import *
from scipy.ndimage import filters
import numpy

im = array(Image.open('board.jpeg').convert('L'))
gray()

subplot(2, 2, 1)
imshow(im)

sigma = 5

imx = zeros(im.shape)
filters.gaussian_filter(im, sigma, (0, 1), imx)
subplot(2, 2, 2)
imshow(imx)

imy = zeros(im.shape)
filters.gaussian_filter(im, sigma, (1, 0), imy)
subplot(2, 2, 3)
imshow(imy)

# there's also gaussian_gradient_magnitude()
mag = numpy.sqrt(imx**2 + imy**2)
subplot(2, 2, 4)
imshow(mag)

show()
