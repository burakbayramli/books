from pylab import *
from scipy.ndimage import filters
import numpy
import rof

im = numpy.zeros((500, 500))
im[100:400, 100:400] = 128
im[200:300, 200:300] = 255
im_noise = im + 30 * numpy.random.standard_normal((500, 500))

U, T = rof.denoise(im_noise, im_noise, tv_weight=100)
G = filters.gaussian_filter(im_noise, 10)

gray()
subplot(2, 2, 1)
imshow(im)

subplot(2, 2, 2)
imshow(im_noise)

subplot(2, 2, 3)
imshow(U)

subplot(2, 2, 4)
imshow(G)

show()
