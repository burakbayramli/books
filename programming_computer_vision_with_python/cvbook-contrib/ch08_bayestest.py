from numpy import array, vstack
import cPickle as pickle
from pylab import *

import imtools
import bayes

"""After ch08_makepoints.py has created test data, this trains a Bayes classifer
and tests how it does."""

def process(training_file, test_file, check, draw):
  # Load training data.
  with open(training_file) as f:
    class_1 = array(pickle.load(f))
    class_2 = array(pickle.load(f))
    labels = pickle.load(f)
  model = bayes.BayesClassifier()
  model.train([class_1, class_2], [1, -1])

  # Load test data.
  with open(test_file) as f:
    class_1 = array(pickle.load(f))
    class_2 = array(pickle.load(f))
    labels = pickle.load(f)

  if check:
    n = class_1.shape[0]
    n_correct = 0
    n_correct += sum(model.classify(class_1)[0] == labels[:n])
    n_correct += sum(model.classify(class_2)[0] == labels[n:])
    print 'percent correct:', 100 * n_correct / float(2 * n)

  if draw:
    def classify(x, y, model=model):
      points = vstack((x, y))
      return model.classify(points.T)[0]
    imtools.plot_2d_boundary(
        [-6, 6, -6, 6], [class_1, class_2], classify, [1, -1])
    show()

process('points_normal.pkl', 'points_normal_test.pkl', check=True, draw=True)
#process('points_ring.pkl', 'points_ring_test.pkl', check=True, draw=True)
