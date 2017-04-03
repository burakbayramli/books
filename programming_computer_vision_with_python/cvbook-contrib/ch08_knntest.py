from numpy import array, vstack
import cPickle as pickle
from pylab import *

import imtools
import knn

"""After ch08_makepoints.py has created test data, this trains a kNN classifer
and tests how it does."""

def process(training_file, test_file, check, draw):
  # Load training data.
  with open(training_file) as f:
    class_1 = pickle.load(f)
    class_2 = pickle.load(f)
    labels = pickle.load(f)
  model = knn.KnnClassifier(labels, vstack((class_1, class_2)))

  # Load test data.
  with open(test_file) as f:
    class_1 = pickle.load(f)
    class_2 = pickle.load(f)
    labels = pickle.load(f)

  if check:
    n = class_1.shape[0]
    n_correct = 0
    for i in range(n):
      if model.classify(class_1[i]) == labels[i]: n_correct += 1
      if model.classify(class_2[i]) == labels[n + i]: n_correct += 1
    print 'percent correct:', 100 * n_correct / float(2 * n)

  if draw:
    def classify(x, y, model=model):
      return array([model.classify([xx, yy]) for (xx, yy) in zip(x, y)])
    imtools.plot_2d_boundary(
        [-6, 6, -6, 6], [class_1, class_2], classify, [1, -1])
    show()

process('points_normal.pkl', 'points_normal_test.pkl', check=False, draw=True)
#process('points_ring.pkl', 'points_ring_test.pkl', check=True, draw=True)
