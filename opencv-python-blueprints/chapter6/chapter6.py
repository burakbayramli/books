#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""OpenCV with Python Blueprints
    Chapter 6: Learning to Recognize Traffic Signs

    Traffic sign recognition using support vector machines (SVMs).
    SVMs are extended for multi-class classification using the "one-vs-one"
    and "one-vs-all" strategies.
"""

import numpy as np
import matplotlib.pyplot as plt

from datasets import gtsrb
from classifiers import MultiClassSVM

__author__ = "Michael Beyeler"
__license__ = "GNU GPL 3.0 or later"


def main():
    strategies = ['one-vs-one', 'one-vs-all']
    features = [None, 'gray', 'rgb', 'hsv', 'surf', 'hog']
    accuracy = np.zeros((2, len(features)))
    precision = np.zeros((2, len(features)))
    recall = np.zeros((2, len(features)))

    for f in xrange(len(features)):
        print "feature", features[f]
        (X_train, y_train), (X_test, y_test) = gtsrb.load_data(
            "datasets/gtsrb_training",
            feature=features[f],
            test_split=0.2,
            seed=42)

        # convert to numpy
        X_train = np.squeeze(np.array(X_train)).astype(np.float32)
        y_train = np.array(y_train)
        X_test = np.squeeze(np.array(X_test)).astype(np.float32)
        y_test = np.array(y_test)

        # find all class labels
        labels = np.unique(np.hstack((y_train, y_test)))

        for s in xrange(len(strategies)):
            print " - strategy", strategies[s]
            # set up SVMs
            MCS = MultiClassSVM(len(labels), strategies[s])

            # training phase
            print "    - train"
            MCS.fit(X_train, y_train)

            # test phase
            print "    - test"
            acc, prec, rec = MCS.evaluate(X_test, y_test)
            accuracy[s, f] = acc
            precision[s, f] = np.mean(prec)
            recall[s, f] = np.mean(rec)
            print "       - accuracy: ", acc
            print "       - mean precision: ", np.mean(prec)
            print "       - mean recall: ", np.mean(rec)

    # plot results as stacked bar plot
    f, ax = plt.subplots(2)
    for s in xrange(len(strategies)):
        x = np.arange(len(features))
        ax[s].bar(x - 0.2, accuracy[s, :], width=0.2, color='b',
                  hatch='/', align='center')
        ax[s].bar(x, precision[s, :], width=0.2, color='r', hatch='\\',
                  align='center')
        ax[s].bar(x + 0.2, recall[s, :], width=0.2, color='g', hatch='x',
                  align='center')
        ax[s].axis([-0.5, len(features) + 0.5, 0, 1.5])
        ax[s].legend(('Accuracy', 'Precision', 'Recall'), loc=2, ncol=3,
                     mode='expand')
        ax[s].set_xticks(np.arange(len(features)))
        ax[s].set_xticklabels(features)
        ax[s].set_title(strategies[s])

    plt.show()


if __name__ == '__main__':
    main()
