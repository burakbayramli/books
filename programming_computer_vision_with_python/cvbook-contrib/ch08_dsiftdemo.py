from PIL import Image
from pylab import *

import dsift, sift

f = '/Users/thakis/Downloads/data/empire.jpg'
dsift.process_image_dsift(f, f + '.sift', 90, 40, True)
loc, desc = sift.read_features_from_file(f + '.sift')

im = array(Image.open(f))
sift.plot_features(im, loc, True)
show()
