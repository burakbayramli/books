from itertools import combinations
import numpy


class ClusterNode(object):
  def __init__(self, vec, left, right, distance=0.0, count=1):
    self.left = left
    self.right = right
    self.vec = vec
    self.distance = distance
    self.count = count

  def extract_clusters(self, dist):
    '''Extract list of subtree clusters with distance < dist.'''
    if self.distance < dist:
      return [self]
    return self.left.extract_clusters(dist) + self.right.extract_clusters(dist)

  def get_cluster_elements(self):
    '''Return ids for elements in a cluster subtree.'''
    return self.left.get_cluster_elements() + self.right.get_cluster_elements()

  def get_height(self):
    '''Return the height of a node.'''
    return self.left.get_height() + self.right.get_height()

  def get_depth(self):
    '''Return the depth of a node.'''
    return max(self.left.get_depth(), self.right.get_depth()) + self.distance

  def draw(self, draw, x, y, s, imlist, im):
    '''Draw nodes recursively with iages for leaf nodes.'''

    # n^2 :-/
    h1 = int(self.left.get_height() * 20 / 2)
    h2 = int(self.right.get_height() * 20 / 2)
    top = y - (h1 + h2)
    bottom = y + (h1 + h2)

    draw.line((x, top + h1, x, bottom - h2), fill=(0, 0, 0))

    l1 = self.distance * s
    draw.line((x, top + h1, x + l1, top + h1), fill=(0, 0, 0))
    draw.line((x, bottom - h2, x + l1, bottom - h2), fill=(0, 0, 0))

    self.left.draw(draw, x + l1, top + h1, s, imlist, im)
    self.right.draw(draw, x + l1, bottom - h2, s, imlist, im)


class ClusterLeafNode(object):
  def __init__(self, vec, id):
    self.vec = vec
    self.id = id

  def extract_clusters(self, dist):
    return [self]

  def get_cluster_elements(self):
    return [self.id]

  def get_height(self):
    return 1

  def get_depth(self):
    return 0

  def draw(self, draw, x, y, s, imlist, im):
    from PIL import Image
    nodeim = Image.open(imlist[self.id])
    nodeim.thumbnail((20, 20))
    ns = nodeim.size
    im.paste(nodeim, [int(x), int(y - ns[1] // 2),
                      int(x + ns[0]), int(y + ns[1] - ns[1] // 2)])


def l2dist(v1, v2):
  return numpy.sqrt(numpy.sum((v1 - v2) ** 2))

def l1dist(v1, v2):
  return numpy.sum(numpy.abs(v1 - v2))


def hcluster(features, distfn=l2dist):
  '''Cluster the rows of features using hiearchical clustering.'''

  distances = {}

  node = [ClusterLeafNode(numpy.array(f), id=i) for i, f in enumerate(features)]

  # O(n^3)
  while len(node) > 1:
    closest = float('Inf')

    # Find closest pair.
    for ni, nj in combinations(node, 2):
      if (ni, nj) not in distances:
        distances[ni, nj] = distfn(ni.vec, nj.vec)

      d = distances[ni, nj]
      if d < closest:
        closest = d
        lowestpair = ni, nj
    ni, nj = lowestpair

    # Combine closest pair.
    new_vec = (ni.vec + nj.vec) / 2.0

    new_node = ClusterNode(new_vec, left=ni, right=nj, distance=closest)
    node.remove(ni)
    node.remove(nj)
    node.append(new_node)

  return node[0]


def draw_dendrogram(node, imlist, filename='out_clusters.png'):
  '''Draw a clsuter dendrogram and save it.'''
  from PIL import Image, ImageDraw

  rows = node.get_height() * 20
  cols = 1200

  s = float(cols - 150) / node.get_depth()

  im = Image.new('RGB', (cols, rows), (255, 255, 255))
  draw = ImageDraw.Draw(im)

  draw.line((0, rows/2, 20, rows/2), fill=(0, 0, 0))
  node.draw(draw, 20, rows / 2, s, imlist, im)
  im.save(filename)
  im.show()
