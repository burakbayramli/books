import numpy

class KnnClassifier(object):
  def __init__(self, labels, samples):
    """Initialize classifier with training data."""
    self.labels = labels
    self.samples = samples

  def classify(self, point, k=3):
    """Classify a point against k nearest in the training data, return label."""
    dist = numpy.array([L2dist(point, s) for s in self.samples])

    ndx = dist.argsort()

    votes = {}
    for i in range(k):
      label = self.labels[ndx[i]]
      votes.setdefault(label, 0)
      votes[label] += 1

    return max(votes, key=votes.get)


def L2dist(p1, p2):
  return numpy.sqrt(numpy.sum((p1 - p2)**2))
