from pylab import *
from scipy import misc
import numpy
import rof

im = misc.lena()
#im = im + 20 * numpy.random.standard_normal(im.shape)

U, T = rof.denoise(im, im, tv_weight=30, tolerance=0.01)

gray()
subplot(1, 2, 1)
imshow(im)

subplot(1, 2, 2)
imshow(U)

show()

