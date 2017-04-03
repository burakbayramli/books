from PIL import Image
from pylab import *
import imtools

im = array(Image.open('coffee.jpeg').convert('L'))
im2, cdf = imtools.histeq(im)

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

show()
