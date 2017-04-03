from PIL import Image
from numpy import *
from pylab import *
import imtools
import pca

# Uses sparse pca codepath.
#imlist = imtools.get_imlist('/Users/thakis/Downloads/data/a_selected_thumbs')

# Uses dense pca codepath.
imlist = imtools.get_imlist('/Users/thakis/Downloads/data/a_thumbs')

im = array(Image.open(imlist[0]))
m, n = im.shape[0:2]
imcount = len(imlist)

immatrix = array([array(Image.open(im)).flatten() for im in imlist], 'f')
V, S, immean = pca.pca(immatrix)

figure()
gray()
subplot(2, 4, 1)
imshow(immean.reshape(m, n))
for i in range(7):
  subplot(2, 4, i + 2)
  imshow(V[i].reshape(m, n))

show()
