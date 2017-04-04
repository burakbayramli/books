#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""OpenCV with Python Blueprints
    Chapter 5: Tracking Visually Salient Objects

    An app to track multiple visually salient objects in a video sequence.
"""

import cv2
import numpy as np
from os import path

from saliency import Saliency
from tracking import MultipleObjectsTracker


def main(video_file='soccer.avi', roi=((140, 100), (500, 600))):
    # open video file
    if path.isfile(video_file):
        video = cv2.VideoCapture(video_file)
    else:
        print 'File "' + video_file + '" does not exist.'
        raise SystemExit

    # initialize tracker
    mot = MultipleObjectsTracker()

    while True:
        # grab next frame
        success, img = video.read()
        if success:
            if roi:
                # original video is too big: grab some meaningful ROI
                img = img[roi[0][0]:roi[1][0], roi[0][1]:roi[1][1]]

            # generate saliency map
            sal = Saliency(img, use_numpy_fft=False, gauss_kernel=(3, 3))

            cv2.imshow('original', img)
            cv2.imshow('saliency', sal.get_saliency_map())
            cv2.imshow('objects', sal.get_proto_objects_map(use_otsu=False))
            cv2.imshow('tracker', mot.advance_frame(img,
                       sal.get_proto_objects_map(use_otsu=False)))

            if cv2.waitKey(100) & 0xFF == ord('q'):
                break
        else:
            break


if __name__ == '__main__':
    main()
