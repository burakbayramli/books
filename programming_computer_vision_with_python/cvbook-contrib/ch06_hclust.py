import hcluster
from pylab import *

class1 = 1.5 * randn(100, 2)
class2 = randn(100, 2) + array([5, 5])
features = vstack((class1, class2))

tree = hcluster.hcluster(features)

clusters = tree.extract_clusters(dist=5)

print 'Number of clusters', len(clusters)
for c in clusters:
  print c.get_cluster_elements()
