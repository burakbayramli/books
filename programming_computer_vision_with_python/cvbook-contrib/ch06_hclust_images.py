import hcluster
import imtools
from PIL import Image
from pylab import *

imlist = imtools.get_imlist('/Users/thakis/Downloads/data/flickr-sunsets-small')

# extract histogram as feature vector (8 bins per color channel)
features = zeros([len(imlist), 512])
for i, f in enumerate(imlist):
  im = array(Image.open(f))

  h, edges = histogramdd(im.reshape(-1, 3), 8, normed=True,
                         range=[(0,255), (0, 255), (0, 255)])
  features[i] = h.flatten()

tree = hcluster.hcluster(features)
hcluster.draw_dendrogram(tree, imlist, filename='out_sunset.png')

# visualize clusters
clusters = tree.extract_clusters(dist=0.23 * tree.distance)
for c in clusters:
  elements = c.get_cluster_elements()
  if len(elements) > 3:
    figure()
    for p in range(minimum(len(elements), 20)):
      subplot(4, 5, p + 1)
      im = array(Image.open(imlist[elements[p]]))
      imshow(im)
      axis('off')
show()
