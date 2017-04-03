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

# spectral clustering
n = len(projected)

# compute distance matrix
S = array([[sqrt(sum((projected[i] - projected[j])**2))
          for i in range(n)] for j in range(n)], 'f')

# create Laplacian matrix
# (See "A Comparison of Spectral Clustering Algorithms", NJW algorithm
rowsum = sum(S, axis=0)
D = diag(1 / sqrt(rowsum))
I = identity(n)
L = I - dot(D, dot(S, D))
#L = dot(dot(D, D), S)  # Ch06, Exercise 5

# compute eigenvectors of L
U,sigma,V = linalg.svd(L)

K = 5
# create feature vector from k first eigenvectors
# by stacking eigenvectors as columns
features = array(V[:K]).T

# k-means
features = whiten(features)
centroids, distortion = kmeans(features, K)
code, distance = vq(features, centroids)

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

