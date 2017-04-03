from PIL import Image
from pylab import *
from scipy.ndimage import filters
from scipy import misc
import imtools

#im = array(Image.open('board.jpeg').convert('L'))
im = misc.lena()
im2, cdf = imtools.histeq(im)
im3 = uint8(255 * (im / (filters.gaussian_filter(im, 400) + 0.00001)))

figure()
gray()
title('original')
imshow(im)

figure()
title('original hist')
#hist(im.flatten(), 128, cumulative=True, normed=True)
hist(im.flatten(), 128, normed=True)

figure()
gray()
title('histogram-equalized')
imshow(im2)

figure()
title('equalized hist')
#hist(im2.flatten(), 128, cumulative=True, normed=True)
hist(im2.flatten(), 128, normed=True)

figure()
gray()
title('quotient image')
imshow(im3)

figure()
title('quotient hist')
hist(im3.flatten(), 128, normed=True)

show()
