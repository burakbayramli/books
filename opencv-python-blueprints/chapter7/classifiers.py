#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""A module that contains various classifiers"""

import cv2
import numpy as np

from abc import ABCMeta, abstractmethod
from matplotlib import pyplot as plt

__author__ = "Michael Beyeler"
__license__ = "GNU GPL 3.0 or later"


class Classifier:
    """
        Abstract base class for all classifiers

        A classifier needs to implement at least two methods:
        - fit:       A method to train the classifier by fitting the model to
                     the data.
        - evaluate:  A method to test the classifier by predicting labels of
                     some test data based on the trained model.

        A classifier also needs to specify a classification strategy via 
        setting self.mode to either "one-vs-all" or "one-vs-one".
        The one-vs-all strategy involves training a single classifier per
        class, with the samples of that class as positive samples and all
        other samples as negatives.
        The one-vs-one strategy involves training a single classifier per
        class pair, with the samples of the first class as positive samples
        and the samples of the second class as negative samples.

        This class also provides method to calculate accuracy, precision,
        recall, and the confusion matrix.
    """
    __metaclass__ = ABCMeta

    @abstractmethod
    def fit(self, X_train, y_train):
        pass

    @abstractmethod
    def evaluate(self, X_test, y_test, visualize=False):
        pass

    def _accuracy(self, y_test, Y_vote):
        """Calculates accuracy

            This method calculates the accuracy based on a vector of
            ground-truth labels (y_test) and a 2D voting matrix (Y_vote) of
            size (len(y_test), num_classes).

            :param y_test: vector of ground-truth labels
            :param Y_vote: 2D voting matrix (rows=samples, cols=class votes)
            :returns: accuracy e[0,1]
        """
        # predicted classes
        y_hat = np.argmax(Y_vote, axis=1)

        # all cases where predicted class was correct
        mask = y_hat == y_test
        return np.count_nonzero(mask)*1. / len(y_test)

    def _precision(self, y_test, Y_vote):
        """Calculates precision

            This method calculates precision extended to multi-class
            classification by help of a confusion matrix.

            :param y_test: vector of ground-truth labels
            :param Y_vote: 2D voting matrix (rows=samples, cols=class votes)
            :returns: precision e[0,1]
        """
        # predicted classes
        y_hat = np.argmax(Y_vote, axis=1)

        if self.mode == "one-vs-one":
            # need confusion matrix
            conf = self._confusion(y_test, Y_vote)

            # consider each class separately
            prec = np.zeros(self.num_classes)
            for c in xrange(self.num_classes):
                # true positives: label is c, classifier predicted c
                tp = conf[c, c]

                # false positives: label is c, classifier predicted not c
                fp = np.sum(conf[:, c]) - conf[c, c]

                # precision
                if tp + fp != 0:
                    prec[c] = tp * 1. / (tp + fp)
        elif self.mode == "one-vs-all":
            # consider each class separately
            prec = np.zeros(self.num_classes)
            for c in xrange(self.num_classes):
                # true positives: label is c, classifier predicted c
                tp = np.count_nonzero((y_test == c) * (y_hat == c))

                # false positives: label is c, classifier predicted not c
                fp = np.count_nonzero((y_test == c) * (y_hat != c))

                if tp + fp != 0:
                    prec[c] = tp * 1. / (tp + fp)
        return prec

    def _recall(self, y_test, Y_vote):
        """Calculates recall
            This method calculates recall extended to multi-class
            classification by help of a confusion matrix.

            :param y_test: vector of ground-truth labels
            :param Y_vote: 2D voting matrix (rows=samples, cols=class votes)
            :returns: recall e[0,1]
        """
        # predicted classes
        y_hat = np.argmax(Y_vote, axis=1)

        if self.mode == "one-vs-one":
            # need confusion matrix
            conf = self._confusion(y_test, Y_vote)

            # consider each class separately
            recall = np.zeros(self.num_classes)
            for c in xrange(self.num_classes):
                # true positives: label is c, classifier predicted c
                tp = conf[c, c]

                # false negatives: label is not c, classifier predicted c
                fn = np.sum(conf[c, :]) - conf[c, c]
                if tp + fn != 0:
                    recall[c] = tp * 1. / (tp + fn)
        elif self.mode == "one-vs-all":
            # consider each class separately
            recall = np.zeros(self.num_classes)
            for c in xrange(self.num_classes):
                # true positives: label is c, classifier predicted c
                tp = np.count_nonzero((y_test == c) * (y_hat == c))

                # false negatives: label is not c, classifier predicted c
                fn = np.count_nonzero((y_test != c) * (y_hat == c))

                if tp + fn != 0:
                    recall[c] = tp * 1. / (tp + fn)
        return recall

    def _confusion(self, y_test, Y_vote):
        """Calculates confusion matrix

            This method calculates the confusion matrix based on a vector of
            ground-truth labels (y-test) and a 2D voting matrix (Y_vote) of
            size (len(y_test), num_classes).
            Matrix element conf[r,c] will contain the number of samples that
            were predicted to have label r but have ground-truth label c.

            :param y_test: vector of ground-truth labels
            :param Y_vote: 2D voting matrix (rows=samples, cols=class votes)
            :returns: confusion matrix
        """
        y_hat = np.argmax(Y_vote, axis=1)
        conf = np.zeros((self.num_classes, self.num_classes)).astype(np.int32)
        for c_true in xrange(self.num_classes):
            # looking at all samples of a given class, c_true
            # how many were classified as c_true? how many as others?
            for c_pred in xrange(self.num_classes):
                y_this = np.where((y_test == c_true) * (y_hat == c_pred))
                conf[c_pred, c_true] = np.count_nonzero(y_this)
        return conf


class MultiLayerPerceptron(Classifier):
    """Multi-Layer Perceptron

        This class implements a multi-layer perceptron (MLP) for multi-class
        classification.

        The size of the input layer of the MLP must equal the number of
        features of the preprocessed data. The size of the output layer
        of the MLP must equal the number of classes used in classification.
    """

    def __init__(self, layer_sizes, class_labels, params=None, 
                 class_mode="one-vs-all"):
        """Constructor

            The constructor initializes the MLP.

            :param layer_sizes:   array of layer sizes [input, hidden,
                                  output]
            :param class_labels:  vector of human-readable (string) class
                                  labels
            :param class_mode:    Classification mode:
                                  - "one-vs-all": The one-vs-all strategy 
                                    involves training a single classifier per 
                                    class, with the samples of that class as 
                                    positive samples and all other samples as 
                                    negatives.
                                  - "one-vs-one": The one-vs-one strategy 
                                    involves training a single classifier per
                                    class pair, with the samples of the first 
                                    class as positive samples and the samples 
                                    of the second class as negative samples.
            :param params:        MLP training parameters.
                                  For now, default values are used.
                                  Hyperparamter exploration can be achieved
                                  by embedding the MLP process flow in a
                                  for-loop that classifies the data with
                                  different parameter values, then pick the
                                  values that yield the best accuracy.
                                  Default: None
        """
        self.num_features = layer_sizes[0]
        self.num_classes = layer_sizes[-1]
        self.class_labels = class_labels
        self.params = params or dict()
        self.mode = class_mode

        # initialize MLP
        self.model = cv2.ANN_MLP()
        self.model.create(layer_sizes)

    def load(self, file):
        """Loads a pre-trained MLP from file"""
        self.model.load(file)

    def save(self, file):
        """Saves a trained MLP to file"""
        self.model.save(file)

    def fit(self, X_train, y_train, params=None):
        """Fits the model to training data

            This method trains the classifier on data (X_train).

            :param X_train: input data (rows=samples, cols=features)
            :param y_train: vector of class labels
            :param params:  dict to specify training options for cv2.MLP.train
                            leave blank to use the parameters passed to the
                            constructor
        """
        if params is None:
            params = self.params

        # need int labels as 1-hot code
        y_train = self._labels_str_to_num(y_train)
        y_train = self._one_hot(y_train).reshape(-1, self.num_classes)

        # train model
        self.model.train(X_train, y_train, None, params=params)

    def predict(self, X_test):
        """Predicts the labels of some test data

            This method predicts the label of some test data and returns the
            predicted labels in human-readable (string) form.

            :param X_test:   input data (rows=samples, cols=features)
            :returns:        predicted string label for every test sample
        """
        ret, y_hat = self.model.predict(X_test)

        # find the most active cell in the output layer
        y_hat = np.argmax(y_hat, 1)

        # return string labels
        return self.__labels_num_to_str(y_hat)

    def evaluate(self, X_test, y_test):
        """Evaluates the model on test data

            This method evaluates the classifier's performance on test data
            (X_test).

            :param X_test:    input data (rows=samples, cols=features)
            :param y_test:    vector of class labels
            :returns: (accuracy, precision, recall)
        """
        # need int labels
        y_test = self._labels_str_to_num(y_test)

        # predict labels
        ret, Y_vote = self.model.predict(X_test)

        accuracy = self._accuracy(y_test, Y_vote)
        precision = self._precision(y_test, Y_vote)
        recall = self._recall(y_test, Y_vote)

        return (accuracy, precision, recall)

    def _one_hot(self, y_train):
        """Converts a list of labels into a 1-hot code"""
        numSamples = len(y_train)
        new_responses = np.zeros(numSamples*self.num_classes, np.float32)
        resp_idx = np.int32(y_train + np.arange(numSamples)*self.num_classes)
        new_responses[resp_idx] = 1
        return new_responses

    def _labels_str_to_num(self, labels):
        """Converts a list of string labels to their corresponding ints"""
        return np.array([int(np.where(self.class_labels == l)[0])
                         for l in labels])

    def __labels_num_to_str(self, labels):
        """Converts a list of int labels to their corresponding strings"""
        return self.class_labels[labels]
