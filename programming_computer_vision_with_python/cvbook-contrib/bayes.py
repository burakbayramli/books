import numpy

class BayesClassifier(object):
  def __init__(self):
    """Initialize classifier with training data."""
    self.labels = []
    self.mean = []
    self.var = []
    self.n = 0

  def train(self, data, labels=None):
    """Train on data (list of arrays n * dim). labels default to 0...n-1."""
    if labels is None:
      labels = range(len(data))
    self.labels = labels
    self.n = len(labels)
    for c in data:
      self.mean.append(numpy.mean(c, axis=0))
      self.var.append(numpy.var(c, axis=0))

  def classify(self, points):
    """Classify points by computing probabilities for each class and returning
    the most probable label."""
    est_prob = numpy.array([gauss(m, v, points)
                            for m, v in zip(self.mean, self.var)])
    ndx = est_prob.argmax(axis=0)
    est_labels = numpy.array([self.labels[n] for n in ndx])
    return est_labels, est_prob


def gauss(m, v, x):
  """Evaluate Gaussian in d dimensions with mean m and variance v at the
  points in the rows of x."""
  if len(x.shape) == 1:
    n, d = 1, x.shape[0]
  else:
    n, d = x.shape
  S = numpy.diag(1 / v)
  x = x - m
  y = numpy.exp(-0.5 * numpy.diag(numpy.dot(x, numpy.dot(S, x.T))))
  return y * (2 * numpy.pi)**(-d / 2.0) / (numpy.sqrt(numpy.prod(v)) + 1e-6)
