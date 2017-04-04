#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""OpenCV with Python Blueprints
    Chapter 2: Hand Gesture Recognition Using a Kinect Depth Sensor

    An app to detect and track simple hand gestures in real-time using the
    output of a Microsoft Kinect 3D Sensor.
"""

import numpy as np

import wx
import cv2
import freenect

from gui import BaseLayout
from gestures import HandGestureRecognition

__author__ = "Michael Beyeler"
__license__ = "GNU GPL 3.0 or later"


class KinectLayout(BaseLayout):
    """Custom layout for Kinect display

        A plain GUI layout for Kinect output. We overwrite the BaseLayout's
        _acquire_frame method to acquire a new frame from the depth sensor
        instead.
    """

    def _init_custom_layout(self):
        """Initializes hand gesture recognition"""
        self.hand_gestures = HandGestureRecognition()

    def _create_custom_layout(self):
        """Use plain layout"""
        pass

    def _acquire_frame(self):
        """Acquire frame from depth sensor using freenect library"""
        frame, _ = freenect.sync_get_depth()
        # return success if frame size is valid
        if frame is not None:
            return (True, frame)
        else:
            return (False, frame)

    def _process_frame(self, frame):
        """Recognizes hand gesture in a frame of the depth sensor"""
        # clip max depth to 1023, convert to 8-bit grayscale
        np.clip(frame, 0, 2**10 - 1, frame)
        frame >>= 2
        frame = frame.astype(np.uint8)

        # recognize hand gesture
        num_fingers, img_draw = self.hand_gestures.recognize(frame)

        # draw some helpers for correctly placing hand
        height, width = frame.shape[:2]
        cv2.circle(img_draw, (width / 2, height / 2), 3, [255, 102, 0], 2)
        cv2.rectangle(img_draw, (width / 3, height / 3), (width * 2 / 3, height * 2 / 3),
                      [255, 102, 0], 2)

        # print number of fingers on image
        cv2.putText(img_draw, str(num_fingers), (30, 30),
                    cv2.FONT_HERSHEY_SIMPLEX, 1, (255, 255, 255))

        return img_draw


def main():
    device = cv2.cv.CV_CAP_OPENNI
    capture = cv2.VideoCapture()
    if not(capture.isOpened()):
        capture.open(device)

    if hasattr(cv2, 'cv'):
        capture.set(cv2.cv.CV_CAP_PROP_FRAME_WIDTH, 640)
        capture.set(cv2.cv.CV_CAP_PROP_FRAME_HEIGHT, 480)
    else:
        capture.set(cv2.CAP_PROP_FRAME_WIDTH, 640)
        capture.set(cv2.CAP_PROP_FRAME_HEIGHT, 480)

    # start graphical user interface
    app = wx.App()
    layout = KinectLayout(capture, title='Kinect Hand Gesture Recognition')
    layout.Show(True)
    app.MainLoop()


if __name__ == '__main__':
    main()
