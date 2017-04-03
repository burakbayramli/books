import hcluster
import imtools
import pca
from PIL import Image
from pylab import *
from scipy.cluster.vq import *

# PCA on all images.
imlist = imtools.get_imlist('/Users/thakis/Downloads/data/a_thumbs')
imcount = len(imlist)
immatrix = array([array(Image.open(im)).flatten() for im in imlist], 'f')
V, S, immean = pca.pca(immatrix)

# Visualize only selected images.
imlist = imtools.get_imlist('/Users/thakis/Downloads/data/a_selected_thumbs')
imcount = len(imlist)
immatrix = array([array(Image.open(im)).flatten() for im in imlist], 'f')

# Project on 40 first PCs.
projected = array([dot(V[:40], immatrix[i] - immean) for i in range(imcount)])

# hierarchical clustering.
tree = hcluster.hcluster(projected)
hcluster.draw_dendrogram(tree, imlist, filename='out_font.png')

# k means.
K = 4
projected = whiten(projected)
centroids, variance = kmeans(projected, K)
code, distance = vq(projected, centroids)

# Plot clusters.
for k in range(K):
  ind = where(code == k)[0]
  figure()
  gray()
  for i in range(minimum(len(ind), 40)):
    subplot(4, 10, i + 1)
    imshow(immatrix[ind[i]].reshape((25, 25)))
    axis('off')
show()
