from pylab import *
from scipy.cluster.vq import *

class1 = 1.5 * randn(100, 2)
class2 = randn(100, 2) + array([5, 5])
features = vstack((class1, class2))

centroids, variance = kmeans(features, 2)

# vq = vector quantize, assign features to centroids
code, distance = vq(features, centroids)

figure()
def plot_class(c, color):
  ndx = where(code == c)[0]
  plot(features[ndx, 0], features[ndx, 1], color)
plot_class(0, '*')
plot_class(1, 'r.')
plot(centroids[:, 0], centroids[:, 1], 'go')
axis('off')
show()
