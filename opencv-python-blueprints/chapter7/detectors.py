#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""A module that contains various detectors"""

import cv2
import numpy as np

__author__ = "Michael Beyeler"
__license__ = "GNU GPL 3.0 or later"


class FaceDetector:
    """Face Detector

        This class implements a face detection algorithm using a face cascade
        and two eye cascades.
    """

    def __init__(
            self,
            face_casc='params/haarcascade_frontalface_default.xml',
            left_eye_casc='params/haarcascade_lefteye_2splits.xml',
            right_eye_casc='params/haarcascade_righteye_2splits.xml',
            scale_factor=4):
        """Initializes cascades

            The constructor initializes all required cascades.
            :param face_casc:         path to a face cascade
            :param left_eye_casc:     path to a left-eye cascade
            :param right_eye_casc:    path to a right-eye cascade
        """
        # resize images before detection
        self.scale_factor = scale_factor

        # load pre-trained cascades
        self.face_casc = cv2.CascadeClassifier(face_casc)
        if self.face_casc.empty():
            print 'Warning: Could not load face cascade:', face_casc
            raise SystemExit
        self.left_eye_casc = cv2.CascadeClassifier(left_eye_casc)
        if self.left_eye_casc.empty():
            print 'Warning: Could not load left eye cascade:', left_eye_casc
            raise SystemExit
        self.right_eye_casc = cv2.CascadeClassifier(right_eye_casc)
        if self.right_eye_casc.empty():
            print 'Warning: Could not load right eye cascade:', right_eye_casc
            raise SystemExit

    def detect(self, frame):
        """Performs face detection

            This method detects faces in an RGB input image.
            The method returns True upon success (else False), draws the
            bounding box of the head onto the input image (frame), and
            extracts the head region (head).

            :param frame: RGB input image
            :returns: success, frame, head
        """
        frameCasc = cv2.cvtColor(
            cv2.resize(
                frame,
                (0, 0),
                fx=1.0 / self.scale_factor,
                fy=1.0 / self.scale_factor),
            cv2.COLOR_RGB2GRAY)
        faces = self.face_casc.detectMultiScale(
            frameCasc,
            scaleFactor=1.1,
            minNeighbors=3,
            flags=cv2.cv.CV_HAAR_FIND_BIGGEST_OBJECT) * self.scale_factor

        # if face is found: extract head region from bounding box
        for (x, y, w, h) in faces:
            cv2.rectangle(frame, (x, y), (x + w, y + h), (100, 255, 0), 2)
            head = cv2.cvtColor(frame[y:y + h, x:x + w],
                                cv2.COLOR_RGB2GRAY)
            return True, frame, head, (x, y)

        return False, frame, None, (0, 0)

    def align_head(self, head):
        """Aligns a head region using affine transformations

            This method preprocesses an extracted head region by rotating
            and scaling it so that the face appears centered and up-right.

            The method returns True on success (else False) and the aligned
            head region (head). Possible reasons for failure are that one or
            both eye detectors fail, maybe due to poor lighting conditions.

            :param head: extracted head region
            :returns: success, head
        """
        height, width = head.shape[:2]

        # detect left eye
        left_eye_region = head[0.2*height:0.5*height, 0.1*width:0.5*width]
        left_eye = self.left_eye_casc.detectMultiScale(
            left_eye_region,
            scaleFactor=1.1,
            minNeighbors=3,
            flags=cv2.cv.CV_HAAR_FIND_BIGGEST_OBJECT)
        left_eye_center = None
        for (xl, yl, wl, hl) in left_eye:
            # find the center of the detected eye region
            left_eye_center = np.array([0.1*width + xl + wl / 2,
                                        0.2*height + yl + hl / 2])
            break  # need only look at first, largest eye

        # detect right eye
        right_eye_region = head[0.2*height:0.5*height, 0.5*width:0.9*width]
        right_eye = self.right_eye_casc.detectMultiScale(
            right_eye_region,
            scaleFactor=1.1,
            minNeighbors=3,
            flags=cv2.cv.CV_HAAR_FIND_BIGGEST_OBJECT)
        right_eye_center = None
        for (xr, yr, wr, hr) in right_eye:
            # find the center of the detected eye region
            right_eye_center = np.array([0.5*width + xr + wr / 2,
                                         0.2*height + yr + hr / 2])
            break  # need only look at first, largest eye

        # need both eyes in order to align face
        # else break here and report failure (False)
        if left_eye_center is None or right_eye_center is None:
            return False, head

        # we want the eye to be at 25% of the width, and 20% of the height
        # resulting image should be square (desired_img_width,
        # desired_img_height)
        desired_eye_x = 0.25
        desired_eye_y = 0.2
        desired_img_width = 200
        desired_img_height = desired_img_width

        # get center point between the two eyes and calculate angle
        eye_center = (left_eye_center + right_eye_center) / 2
        eye_angle_deg = np.arctan2(right_eye_center[1] - left_eye_center[1],
                                   right_eye_center[0] - left_eye_center[0]) \
            * 180.0 / cv2.cv.CV_PI

        # scale distance between eyes to desired length
        eyeSizeScale = (1.0 - desired_eye_x * 2) * desired_img_width / \
            np.linalg.norm(right_eye_center - left_eye_center)

        # get rotation matrix
        rot_mat = cv2.getRotationMatrix2D(tuple(eye_center), eye_angle_deg,
                                          eyeSizeScale)

        # shift center of the eyes to be centered in the image
        rot_mat[0, 2] += desired_img_width*0.5 - eye_center[0]
        rot_mat[1, 2] += desired_eye_y*desired_img_height - eye_center[1]

        # warp perspective to make eyes aligned on horizontal line and scaled
        # to right size
        res = cv2.warpAffine(head, rot_mat, (desired_img_width,
                                             desired_img_width))

        # return success
        return True, res
