#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""A script to train and test a MLP classifier"""

import cv2
import numpy as np

from datasets import homebrew
from classifiers import MultiLayerPerceptron

__author__ = "Michael Beyeler"
__license__ = "GNU GPL 3.0 or later"


def main():
    # load training data
    # training data can be recorded using chapter7.py in training mode
    (X_train, y_train), (X_test, y_test), _, _ = homebrew.load_data(
        "datasets/faces_training.pkl",
        num_components=50,
        test_split=0.2,
        save_to_file="datasets/faces_preprocessed.pkl",
        seed=42)
    if len(X_train) == 0 or len(X_test) == 0:
        print "Empty data"
        raise SystemExit

    # convert to numpy
    X_train = np.squeeze(np.array(X_train)).astype(np.float32)
    y_train = np.array(y_train)
    X_test = np.squeeze(np.array(X_test)).astype(np.float32)
    y_test = np.array(y_test)

    # find all class labels
    labels = np.unique(np.hstack((y_train, y_test)))

    # prepare training
    num_features = len(X_train[0])
    num_classes = len(labels)
    params = dict(term_crit=(cv2.TERM_CRITERIA_COUNT, 300, 0.01),
                  train_method=cv2.ANN_MLP_TRAIN_PARAMS_BACKPROP,
                  bp_dw_scale=0.001, bp_moment_scale=0.9)
    saveFile = 'params/mlp.xml'

    # find best MLP configuration
    print "---"
    print "1-hidden layer networks"
    best_acc = 0.0  # keep track of best accuracy
    for l1 in xrange(10):
        # gradually increase the hidden-layer size
        layerSizes = np.int32([num_features, (l1 + 1) * num_features/5,
                               num_classes])
        MLP = MultiLayerPerceptron(layerSizes, labels)
        print layerSizes
        MLP.fit(X_train, y_train, params=params)
        (acc, _, _) = MLP.evaluate(X_train, y_train)
        print " - train acc = ", acc
        (acc, _, _) = MLP.evaluate(X_test, y_test)
        print " - test acc = ", acc
        if acc > best_acc:
            # save best MLP configuration to file
            MLP.save(saveFile)
            best_acc = acc


if __name__ == '__main__':
    main()
