import glob
import os
import numpy

from libsvm import svmutil

import bayes
import knn
import pca
import sift

"""Run after ch08_preparedata.py has run."""

def read_gesture_feature_labels(path):
  featlist = glob.glob(os.path.join(path, '*.dsift'))
  features = [sift.read_features_from_file(f)[1].flatten() for f in featlist]
  labels = [os.path.basename(f)[0] for f in featlist]
  return numpy.array(features), numpy.array(labels)


def print_confusion(res, labels, classnames):
  n = len(classnames)
  class_ind = dict([(classnames[i], i) for i in range(n)])

  confuse = numpy.zeros((n, n))  # wow
  for i in range(len(test_labels)):
    confuse[class_ind[res[i]], class_ind[test_labels[i]]] += 1

  print 'Confusion matrix for'
  print classnames
  print confuse


features, labels = read_gesture_feature_labels('out_hands/train')
test_features, test_labels = read_gesture_feature_labels('out_hands/test')

classnames = numpy.unique(labels)

# Reduce input dimensions.
V, S, m = pca.pca(features)
V = V[:50]  # Keep most important features.
features = numpy.array([numpy.dot(V, f - m) for f in features])
test_features = numpy.array([numpy.dot(V, f - m) for f in test_features])


# Test kNN.
k = 1
knn_classifier = knn.KnnClassifier(labels, features)

res = numpy.array([knn_classifier.classify(feat, k) for feat in test_features])
acc = numpy.sum(1.0 * (res == test_labels)) / len(test_labels)
print 'kNN Accuracy:', acc
print_confusion(res, test_labels, classnames)


# Test Bayes.
bc = bayes.BayesClassifier()
blist = [features[numpy.where(labels == c)[0]] for c in classnames]
bc.train(blist, classnames)

res = bc.classify(test_features)[0]
acc = numpy.sum(1.0 * (res == test_labels)) / len(test_labels)
print 'Bayes Accuracy:', acc
print_confusion(res, test_labels, classnames)
# FIXME: Bayes accuracy gets very bad if the input dimensions aren't reduced
# enough. Probably some float underflow due to things not using log
# probabilities?


# Test SVM.
features = map(list, features)
test_features = map(list, test_features)

str_int_map = {}  # libSVM needs int labels.
for i, c in enumerate(classnames):
  str_int_map[c], str_int_map[i] = i, c

def convert_labels(labels, str_int_map):
  return [str_int_map[l] for l in labels]

problem = svmutil.svm_problem(convert_labels(labels, str_int_map), features)
# Use a linear kernel, radial basis functions have horrible results (~20% acc)
param = svmutil.svm_parameter('-q -t 0')
model = svmutil.svm_train(problem, param)
res = svmutil.svm_predict(
    convert_labels(test_labels, str_int_map), test_features, model)[0]
res = convert_labels(res, str_int_map)
acc = numpy.sum(1.0 * (res == test_labels)) / len(test_labels)
print 'SVM Accuracy:', acc
print_confusion(res, test_labels, classnames)
