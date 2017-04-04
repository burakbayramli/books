#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""A module that contains an algorithm for multiple-objects tracking"""

import cv2
import numpy as np
import copy


class MultipleObjectsTracker:
    """Multiple-objects tracker

        This class implements an algorithm for tracking multiple objects in
        a video sequence.
        The algorithm combines a saliency map for object detection and
        mean-shift tracking for object tracking.
    """
    def __init__(self, min_area=400, min_shift2=5):
        """Constructor

            This method initializes the multiple-objects tracking algorithm.

            :param min_area: Minimum area for a proto-object contour to be
                             considered a real object
            :param min_shift2: Minimum distance for a proto-object to drift
                               from frame to frame ot be considered a real
                               object
        """
        self.object_roi = []
        self.object_box = []

        self.min_cnt_area = min_area
        self.min_shift2 = min_shift2

        # Setup the termination criteria, either 100 iteration or move by at
        # least 1 pt
        self.term_crit = (cv2.TERM_CRITERIA_EPS | cv2.TERM_CRITERIA_COUNT,
                          100, 1)

    def advance_frame(self, frame, proto_objects_map):
        """Advances the algorithm by a single frame

            This method tracks all objects via the following steps:
             - adds all bounding boxes from saliency map as potential
               targets
             - finds bounding boxes from previous frame in current frame
               via mean-shift tracking
             - combines the two lists by removing duplicates

            certain targets are discarded:
             - targets that are too small
             - targets that don't move

            :param frame: New input RGB frame
            :param proto_objects_map: corresponding proto-objects map of the
                                      frame
            :returns: frame annotated with bounding boxes around all objects
                      that are being tracked
        """
        self.tracker = copy.deepcopy(frame)

        # build a list of all bounding boxes
        box_all = []

        # append to the list all bounding boxes found from the
        # current proto-objects map
        box_all = self._append_boxes_from_saliency(proto_objects_map, box_all)

        # find all bounding boxes extrapolated from last frame
        # via mean-shift tracking
        box_all = self._append_boxes_from_meanshift(frame, box_all)

        # only keep those that are both salient and in mean shift
        if len(self.object_roi) == 0:
            group_thresh = 0  # no previous frame: keep all form saliency
        else:
            group_thresh = 1  # previous frame + saliency
        box_grouped, _ = cv2.groupRectangles(box_all, group_thresh, 0.1)

        # update mean-shift bookkeeping for remaining boxes
        self._update_mean_shift_bookkeeping(frame, box_grouped)

        # draw remaining boxes
        for (x, y, w, h) in box_grouped:
            cv2.rectangle(self.tracker, (x, y), (x + w, y + h),
                          (0, 255, 0), 2)

        return self.tracker

    def _append_boxes_from_saliency(self, proto_objects_map, box_all):
        """Adds to the list all bounding boxes found with the saliency map

            A saliency map is used to find objects worth tracking in each
            frame. This information is combined with a mean-shift tracker
            to find objects of relevance that move, and to discard everything
            else.

            :param proto_objects_map: proto-objects map of the current frame
            :param box_all: append bounding boxes from saliency to this list
            :returns: new list of all collected bounding boxes
        """
        # find all bounding boxes in new saliency map
        box_sal = []
        cnt_sal, _ = cv2.findContours(proto_objects_map, 1, 2)
        for cnt in cnt_sal:
            # discard small contours
            if cv2.contourArea(cnt) < self.min_cnt_area:
                continue

            # otherwise add to list of boxes found from saliency map
            box = cv2.boundingRect(cnt)
            box_all.append(box)

        return box_all

    def _append_boxes_from_meanshift(self, frame, box_all):
        """Adds to the list all bounding boxes found with mean-shift tracking

            Mean-shift tracking is used to track objects from frame to frame.
            This information is combined with a saliency map to discard
            false-positives and focus only on relevant objects that move.

            :param frame: current RGB image frame
            :box_all: append bounding boxes from tracking to this list
            :returns: new list of all collected bounding boxes
        """
        hsv = cv2.cvtColor(frame, cv2.COLOR_BGR2HSV)

        for i in xrange(len(self.object_roi)):
            roi_hist = copy.deepcopy(self.object_roi[i])
            box_old = copy.deepcopy(self.object_box[i])

            dst = cv2.calcBackProject([hsv], [0], roi_hist, [0, 180], 1)
            ret, box_new = cv2.meanShift(dst, tuple(box_old), self.term_crit)
            self.object_box[i] = copy.deepcopy(box_new)

            # discard boxes that don't move
            (xo, yo, wo, ho) = box_old
            (xn, yn, wn, hn) = box_new

            co = [xo + wo/2, yo + ho/2]
            cn = [xn + wn/2, yn + hn/2]
            if (co[0]-cn[0])**2 + (co[1]-cn[1])**2 >= self.min_shift2:
                box_all.append(box_new)

        return box_all

    def _update_mean_shift_bookkeeping(self, frame, box_grouped):
        """Preprocess all valid bounding boxes for mean-shift tracking

            This method preprocesses all relevant bounding boxes (those that
            have been detected by both mean-shift tracking and saliency) for
            the next mean-shift step.

            :param frame: current RGB input frame
            :param box_grouped: list of bounding boxes
        """
        hsv = cv2.cvtColor(frame, cv2.COLOR_BGR2HSV)

        self.object_roi = []
        self.object_box = []
        for box in box_grouped:
            (x, y, w, h) = box
            hsv_roi = hsv[y:y + h, x:x + w]
            mask = cv2.inRange(hsv_roi, np.array((0., 60., 32.)),
                               np.array((180., 255., 255.)))
            roi_hist = cv2.calcHist([hsv_roi], [0], mask, [180], [0, 180])
            cv2.normalize(roi_hist, roi_hist, 0, 255, cv2.NORM_MINMAX)

            self.object_roi.append(roi_hist)
            self.object_box.append(box)
