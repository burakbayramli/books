from numpy import array, vstack
import cPickle as pickle
from pylab import *

from libsvm import svmutil

import imtools

"""After ch08_makepoints.py has created test data, this trains a SVM classifer
and tests how it does."""

def process(training_file, test_file, check, draw):
  # Load training data.
  with open(training_file) as f:
    class_1 = pickle.load(f)
    class_2 = pickle.load(f)
    labels = pickle.load(f)

  # Convert data to lists for libsvm.
  class_1 = map(list, class_1)
  class_2 = map(list, class_2)
  labels = list(labels)
  samples = class_1 + class_2
  problem = svmutil.svm_problem(labels, samples)
  # Don't print to stdout, use radial basis functions.
  param = svmutil.svm_parameter('-q -t 2')
  model = svmutil.svm_train(problem, param)

  # Load test data.
  with open(test_file) as f:
    class_1 = pickle.load(f)
    class_2 = pickle.load(f)
    labels = pickle.load(f)

  class_1 = map(list, class_1)
  class_2 = map(list, class_2)
  labels = list(labels)

  if check:
    # Sadly, this prints to stdout too :-/
    svmutil.svm_predict(labels, class_1 + class_2, model)  # Prints accuracy.

  if draw:
    def classify(x, y, model=model):
      return array(svmutil.svm_predict([0] * len(x), map(list, zip(x, y)),
                                       model)[0])
    imtools.plot_2d_boundary(
        [-6, 6, -6, 6], [array(class_1), array(class_2)], classify, [1, -1])
    show()

process('points_normal.pkl', 'points_normal_test.pkl', check=True, draw=True)
#process('points_ring.pkl', 'points_ring_test.pkl', check=True, draw=True)
