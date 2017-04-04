#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""OpenCV with Python Blueprints
    Chapter 7: Learning to Recognize Emotion in Faces

    An app that combines both face detection and face recognition, with a
    focus on recognizing emotional expressions in the detected faces.

    The process flow is as follows:
    * Run the GUI in Training Mode to assemble a training set. Upon exiting
      the app will dump all assembled training samples to a pickle file
      "datasets/faces_training.pkl".
    * Run the script train_test_mlp.py to train a MLP classifier on the
      dataset. This file will store the parameters of the trained MLP in
      a file "params/mlp.xml" and dump the preprocessed dataset to a
      pickle file "datasets/faces_preprocessed.pkl".
    * Run the GUI in Testing Mode to apply the pre-trained MLP classifier
      to the live stream of the webcam.
"""

import cv2
import numpy as np

import time
import wx
from os import path
import cPickle as pickle

from datasets import homebrew
from detectors import FaceDetector
from classifiers import MultiLayerPerceptron
from gui import BaseLayout

__author__ = "Michael Beyeler"
__license__ = "GNU GPL 3.0 or later"


class FaceLayout(BaseLayout):
    """A custom layout for face detection and facial expression recognition

        A GUI to both assemble a training set and to perform real-time
        classification on the live stream of a webcam using a pre-trained
        classifier.

        The GUI operates in two different modes:
        * Training Mode: In training mode, the app will collect image frames,
                         detect a face therein, assignassign a label depending
                         on the facial expression, and upon exiting save all
                         collected data samples in a file, so that it can be
                         parsed by datasets.homebrew.
        * Testing Mode:  In testing mode, the app will detect a face in each
                         video frame and predict the corresponding class
                         label using a pre-trained MLP.
    """

    def _init_custom_layout(self):
        """Initializes GUI"""
        # initialize data structure
        self.samples = []
        self.labels = []

        # call method to save data upon exiting
        self.Bind(wx.EVT_CLOSE, self._on_exit)

    def init_algorithm(
            self,
            save_training_file='datasets/faces_training.pkl',
            load_preprocessed_data='datasets/faces_preprocessed.pkl',
            load_mlp='params/mlp.xml',
            face_casc='params/haarcascade_frontalface_default.xml',
            left_eye_casc='params/haarcascade_lefteye_2splits.xml',
            right_eye_casc='params/haarcascade_righteye_2splits.xml'):
        """Initializes face detector and facial expression classifier

            This method initializes both the face detector and the facial
            expression classifier.

            :param save_training_file:     filename for storing the assembled
                                           training set
            :param load_preprocessed_data: filename for loading a previously
                                           preprocessed dataset (for
                                           classification in Testing Mode)
            :param load_mlp:               filename for loading a pre-trained
                                           MLP classifier (use the script
                                           train_test_mlp.py)
            :param face_casc:              path to a face cascade
            :param left_eye_casc:          path to a left-eye cascade
            :param right_eye_casc:         path to a right-eye cascade
        """
        self.data_file = save_training_file
        self.faces = FaceDetector(face_casc, left_eye_casc, right_eye_casc)
        self.head = None

        # load preprocessed dataset to access labels and PCA params
        if path.isfile(load_preprocessed_data):
            (_, y_train), (_, y_test), V, m = homebrew.load_from_file(
                load_preprocessed_data)
            self.pca_V = V
            self.pca_m = m
            self.all_labels = np.unique(np.hstack((y_train, y_test)))

            # load pre-trained multi-layer perceptron
            if path.isfile(load_mlp):
                layer_sizes = np.array([self.pca_V.shape[1],
                                        len(self.all_labels)])
                self.MLP = MultiLayerPerceptron(layer_sizes, self.all_labels)
                self.MLP.load(load_mlp)
            else:
                print "Warning: Testing is disabled"
                print "Could not find pre-trained MLP file ", load_mlp
                self.testing.Disable()
        else:
            print "Warning: Testing is disabled"
            print "Could not find data file ", load_preprocessed_data
            self.testing.Disable()

    def _create_custom_layout(self):
        """Decorates the GUI with buttons for assigning class labels"""
        # create horizontal layout with train/test buttons
        pnl1 = wx.Panel(self, -1)
        self.training = wx.RadioButton(pnl1, -1, 'Train', (10, 10),
                                       style=wx.RB_GROUP)
        self.Bind(wx.EVT_RADIOBUTTON, self._on_training, self.training)
        self.testing = wx.RadioButton(pnl1, -1, 'Test')
        self.Bind(wx.EVT_RADIOBUTTON, self._on_testing, self.testing)
        hbox1 = wx.BoxSizer(wx.HORIZONTAL)
        hbox1.Add(self.training, 1)
        hbox1.Add(self.testing, 1)
        pnl1.SetSizer(hbox1)

        # create a horizontal layout with all buttons
        pnl2 = wx.Panel(self, -1)
        self.neutral = wx.RadioButton(pnl2, -1, 'neutral', (10, 10),
                                      style=wx.RB_GROUP)
        self.happy = wx.RadioButton(pnl2, -1, 'happy')
        self.sad = wx.RadioButton(pnl2, -1, 'sad')
        self.surprised = wx.RadioButton(pnl2, -1, 'surprised')
        self.angry = wx.RadioButton(pnl2, -1, 'angry')
        self.disgusted = wx.RadioButton(pnl2, -1, 'disgusted')
        hbox2 = wx.BoxSizer(wx.HORIZONTAL)
        hbox2.Add(self.neutral, 1)
        hbox2.Add(self.happy, 1)
        hbox2.Add(self.sad, 1)
        hbox2.Add(self.surprised, 1)
        hbox2.Add(self.angry, 1)
        hbox2.Add(self.disgusted, 1)
        pnl2.SetSizer(hbox2)

        # create horizontal layout with single snapshot button
        pnl3 = wx.Panel(self, -1)
        self.snapshot = wx.Button(pnl3, -1, 'Take Snapshot')
        self.Bind(wx.EVT_BUTTON, self._on_snapshot, self.snapshot)
        hbox3 = wx.BoxSizer(wx.HORIZONTAL)
        hbox3.Add(self.snapshot, 1)
        pnl3.SetSizer(hbox3)

        # arrange all horizontal layouts vertically
        self.panels_vertical.Add(pnl1, flag=wx.EXPAND | wx.TOP, border=1)
        self.panels_vertical.Add(pnl2, flag=wx.EXPAND | wx.BOTTOM, border=1)
        self.panels_vertical.Add(pnl3, flag=wx.EXPAND | wx.BOTTOM, border=1)

    def _process_frame(self, frame):
        """Processes each captured frame

            This method processes each captured frame.
            * Training mode:  Performs face detection.
            * Testing mode:   Performs face detection, and predicts the class
                              label of the facial expression.
        """
        # detect face
        success, frame, self.head, (x, y) = self.faces.detect(frame)

        if success and self.testing.GetValue():
            # if face found: preprocess (align)
            success, head = self.faces.align_head(self.head)
            if success:
                # extract features using PCA (loaded from file)
                X, _, _ = homebrew.extract_features([head.flatten()],
                                                    self.pca_V, self.pca_m)

                # predict label with pre-trained MLP
                label = self.MLP.predict(np.array(X))[0]

                # draw label above bounding box
                cv2.putText(frame, str(label), (x, y - 20),
                            cv2.FONT_HERSHEY_COMPLEX, 1, (0, 255, 0), 2)

        return frame

    def _on_training(self, evt):
        """Enables all training-related buttons when Training Mode is on"""
        self.neutral.Enable()
        self.happy.Enable()
        self.sad.Enable()
        self.surprised.Enable()
        self.angry.Enable()
        self.disgusted.Enable()
        self.snapshot.Enable()

    def _on_testing(self, evt):
        """Disables all training-related buttons when Testing Mode is on"""
        self.neutral.Disable()
        self.happy.Disable()
        self.sad.Disable()
        self.surprised.Disable()
        self.angry.Disable()
        self.disgusted.Disable()
        self.snapshot.Disable()

    def _on_snapshot(self, evt):
        """Takes a snapshot of the current frame

            This method takes a snapshot of the current frame, preprocesses
            it to extract the head region, and upon success adds the data
            sample to the training set.
        """
        if self.neutral.GetValue():
            label = 'neutral'
        elif self.happy.GetValue():
            label = 'happy'
        elif self.sad.GetValue():
            label = 'sad'
        elif self.surprised.GetValue():
            label = 'surprised'
        elif self.angry.GetValue():
            label = 'angry'
        elif self.disgusted.GetValue():
            label = 'disgusted'

        if self.head is None:
            print "No face detected"
        else:
            success, head = self.faces.align_head(self.head)
            if success:
                print "Added sample to training set"
                self.samples.append(head.flatten())
                self.labels.append(label)
            else:
                print "Could not align head (eye detection failed?)"

    def _on_exit(self, evt):
        """Dumps the training data to file upon exiting"""
        # if we have collected some samples, dump them to file
        if len(self.samples) > 0:
            # make sure we don't overwrite an existing file
            if path.isfile(self.data_file):
                # file already exists, construct new load_from_file
                load_from_file, fileext = path.splitext(self.data_file)
                offset = 0
                while True:
                    file = load_from_file + "-" + str(offset) + fileext
                    if path.isfile(file):
                        offset += 1
                    else:
                        break
                self.data_file = file

            # dump samples and labels to file
            f = open(self.data_file, 'wb')
            pickle.dump(self.samples, f)
            pickle.dump(self.labels, f)
            f.close()

            # inform user that file was created
            print "Saved", len(self.samples), "samples to", self.data_file

        # deallocate
        self.Destroy()


def main():
    capture = cv2.VideoCapture(0)
    if not(capture.isOpened()):
        capture.open()

    if hasattr(cv2, 'cv'):
        capture.set(cv2.cv.CV_CAP_PROP_FRAME_WIDTH, 640)
        capture.set(cv2.cv.CV_CAP_PROP_FRAME_HEIGHT, 480)
    else:
        capture.set(cv2.CAP_PROP_FRAME_WIDTH, 640)
        capture.set(cv2.CAP_PROP_FRAME_HEIGHT, 480)

    # start graphical user interface
    app = wx.App()
    layout = FaceLayout(capture, title='Facial Expression Recognition')
    layout.init_algorithm()
    layout.Show(True)
    app.MainLoop()


if __name__ == '__main__':
    main()
