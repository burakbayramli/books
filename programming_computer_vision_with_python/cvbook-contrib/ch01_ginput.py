from PIL import Image
from pylab import *

im = array(Image.open('coffee.jpeg'))
imshow(im)
print 'click 3 points'
x = ginput(3)
print 'clicked:', x

show()

