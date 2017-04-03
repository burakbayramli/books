from PIL import Image
from pylab import *
from scipy.cluster.vq import *
from scipy.misc import imresize

steps = 50  # image is divided in steps*steps region
im = array(Image.open('board.jpeg'))

dy = im.shape[0] / steps
dx = im.shape[1] / steps

# The feature for each region is the average color of that region.
features = []
for y in range(steps):
  for x in range(steps):
    R = mean(im[y*dy:(y+1)*dy, x*dx:(x+1)*dx, 0])
    G = mean(im[y*dy:(y+1)*dy, x*dx:(x+1)*dx, 1])
    B = mean(im[y*dy:(y+1)*dy, x*dx:(x+1)*dx, 2])
    features.append([R, G, B])
features = array(features, 'f')

# Cluster.
centroids, variance = kmeans(features, 3)
code, distance = vq(features, centroids)

codeim = code.reshape(steps, steps)
codeim = imresize(codeim, im.shape[:2], interp='nearest')

figure()
imshow(codeim)
show()
