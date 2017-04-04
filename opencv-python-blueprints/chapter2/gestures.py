#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""A module containing an algorithm for hand gesture recognition"""

import numpy as np
import cv2

__author__ = "Michael Beyeler"
__license__ = "GNU GPL 3.0 or later"


class HandGestureRecognition:
    """Hand gesture recognition class

        This class implements an algorithm for hand gesture recognition
        based on a single-channel input image showing the segmented arm region,
        where pixel values stand for depth. The easiest way to acquire
        such an image is with a depth sensor such as Microsoft Kinect 3D.

        The algorithm will then find the hull of the segmented hand
        region and convexity defects therein. Based on this information,
        an estimate on the number of extended fingers is derived.
    """

    def __init__(self):
        """Class constructor

            This method initializes all necessary parameters.
        """
        # maximum depth deviation for a pixel to be considered within range
        self.abs_depth_dev = 14

        # cut-off angle (deg): everything below this is a convexity point that
        # belongs to two extended fingers
        self.thresh_deg = 80.0

    def recognize(self, img_gray):
        """Recognizes hand gesture in a single-channel depth image

            This method estimates the number of extended fingers based on
            a single-channel depth image showing a hand and arm region.
            :param img_gray: single-channel depth image
            :returns: (num_fingers, img_draw) The estimated number of
                       extended fingers and an annotated RGB image
        """
        self.height, self.width = img_gray.shape[:2]

        # segment arm region
        segment = self._segment_arm(img_gray)

        # find the hull of the segmented area, and based on that find the
        # convexity defects
        (contours, defects) = self._find_hull_defects(segment)

        # detect the number of fingers depending on the contours and convexity
        # defects, then draw defects that belong to fingers green, others red
        img_draw = cv2.cvtColor(img_gray, cv2.COLOR_GRAY2RGB)
        (num_fingers, img_draw) = self._detect_num_fingers(contours,
                                                           defects, img_draw)

        return (num_fingers, img_draw)

    def _segment_arm(self, frame):
        """Segments arm region

            This method accepts a single-channel depth image of an arm and
            hand region and extracts the segmented arm region.
            It is assumed that the hand is placed in the center of the image.
            :param frame: single-channel depth image
            :returns: binary image (mask) of segmented arm region, where
                      arm=255, else=0
        """
        # find center (21x21 pixel) region of image frame
        center_half = 10  # half-width of 21 is 21/2-1
        center = frame[self.height/2-center_half:self.height/2+center_half,
                       self.width/2-center_half:self.width/2+center_half]

        # find median depth value of center region
        med_val = np.median(center)

        # try this instead:
        frame = np.where(abs(frame-med_val) <= self.abs_depth_dev,
                         128, 0).astype(np.uint8)

        # morphological
        kernel = np.ones((3, 3), np.uint8)
        frame = cv2.morphologyEx(frame, cv2.MORPH_CLOSE, kernel)

        # connected component
        small_kernel = 3
        frame[self.height/2-small_kernel:self.height/2+small_kernel,
              self.width/2-small_kernel:self.width/2+small_kernel] = 128

        mask = np.zeros((self.height+2, self.width+2), np.uint8)
        flood = frame.copy()
        cv2.floodFill(flood, mask, (self.width/2, self.height/2), 255,
                      flags=4 | (255 << 8))

        ret, flooded = cv2.threshold(flood, 129, 255, cv2.THRESH_BINARY)

        return flooded

    def _find_hull_defects(self, segment):
        """Find hull defects

            This method finds all defects in the hull of a segmented arm
            region.
            :param segment: a binary image (mask) of a segmented arm region,
                            where arm=255, else=0
            :returns: (max_contour, defects) the largest contour in the image
                      and all corresponding defects
        """
        contours, hierarchy = cv2.findContours(segment, cv2.RETR_TREE,
                                               cv2.CHAIN_APPROX_SIMPLE)

        # find largest area contour
        max_contour = max(contours, key=cv2.contourArea)
        epsilon = 0.01*cv2.arcLength(max_contour, True)
        max_contour = cv2.approxPolyDP(max_contour, epsilon, True)

        # find convexity hull and defects
        hull = cv2.convexHull(max_contour, returnPoints=False)
        defects = cv2.convexityDefects(max_contour, hull)

        return (max_contour, defects)

    def _detect_num_fingers(self, contours, defects, img_draw):
        """Detects the number of extended fingers

            This method determines the number of extended fingers based on a
            contour and convexity defects.
            It will annotate an RGB color image of the segmented arm region
            with all relevant defect points and the hull.
            :param contours: a list of contours
            :param defects: a list of convexity defects
            :param img_draw: an RGB color image to be annotated
            :returns: (num_fingers, img_draw) the estimated number of extended
                      fingers and an annotated RGB color image
        """

        # if there are no convexity defects, possibly no hull found or no
        # fingers extended
        if defects is None:
            return [0, img_draw]

        # we assume the wrist will generate two convexity defects (one on each
        # side), so if there are no additional defect points, there are no
        # fingers extended
        if len(defects) <= 2:
            return [0, img_draw]

        # if there is a sufficient amount of convexity defects, we will find a
        # defect point between two fingers so to get the number of fingers,
        # start counting at 1
        num_fingers = 1

        for i in range(defects.shape[0]):
            # each defect point is a 4-tuple
            start_idx, end_idx, farthest_idx, _ = defects[i, 0]
            start = tuple(contours[start_idx][0])
            end = tuple(contours[end_idx][0])
            far = tuple(contours[farthest_idx][0])

            # draw the hull
            cv2.line(img_draw, start, end, [0, 255, 0], 2)

            # if angle is below a threshold, defect point belongs to two
            # extended fingers
            if angle_rad(np.subtract(start, far),
                         np.subtract(end, far)) < deg2rad(self.thresh_deg):
                # increment number of fingers
                num_fingers = num_fingers + 1

                # draw point as green
                cv2.circle(img_draw, far, 5, [0, 255, 0], -1)
            else:
                # draw point as red
                cv2.circle(img_draw, far, 5, [255, 0, 0], -1)

        # make sure we cap the number of fingers
        return (min(5, num_fingers), img_draw)


def angle_rad(v1, v2):
    """Angle in radians between two vectors

        This method returns the angle (in radians) between two array-like
        vectors using the cross-product method, which is more accurate for
        small angles than the dot-product-acos method.
    """
    return np.arctan2(np.linalg.norm(np.cross(v1, v2)), np.dot(v1, v2))


def deg2rad(angle_deg):
    """Convert degrees to radians

        This method converts an angle in radians e[0,2*np.pi) into degrees
        e[0,360)
    """
    return angle_deg/180.0*np.pi
