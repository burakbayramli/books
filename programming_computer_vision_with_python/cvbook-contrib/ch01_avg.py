from PIL import Image
from pylab import *
import imtools

avg = imtools.compute_average(['coffee.jpeg', 'board.jpeg', 'sea.jpeg'])

imshow(avg)
show()
