#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""A module containing simple GUI layouts using wxPython"""

from abc import ABCMeta, abstractmethod
import time

import wx
import cv2

__author__ = "Michael Beyeler"
__license__ = "GNU GPL 3.0 or later"


class Meta1(wx.Frame):
    pass


class BaseLayout(Meta1):
    """Abstract base class for all layouts

        A custom layout needs to implement at least three methods:
        * _init_custom_layout:   A method to initialize all relevant
                                 parameters. This method will be called in the
                                 class constructor, after initializing common
                                 parameters, right before creating the GUI
                                 layout.
        * _create_custom_layout: A method to create a custom GUI layout. This
                                 method will be called in the class
                                 constructor, after initializing common
                                 parameters.
                                 Every GUI contains the camera feed in the
                                 variable self.pnl.
                                 Additional layout elements can be added below
                                 the camera feed by means of the method
                                 self.panels_vertical.Add.
        * _process_frame:        A method to process the current RGB camera
                                 frame. It needs to return the processed RGB
                                 frame to be displayed.
    """
    __metaclass__ = ABCMeta

    def __init__(self, capture, title=None, parent=None, id=-1, fps=10):
        """Class constructor

            This method initializes all necessary parameters and generates a
            basic GUI layout that can then be modified by
            self.init_custom_layout() and self.create_custom_layout().

            :param parent: A wx.Frame parent (often Null). If it is non-Null,
                the frame will be minimized when its parent is minimized and
                restored when it is restored.
            :param id: The window identifier. Value -1 indicates default value.
            :param title: The caption to be displayed on the frame's title bar.
            :param capture: A cv2.VideoCapture object to be used as camera
                feed.
            :param fps: frames per second at which to display camera feed
        """
        self.capture = capture
        self.fps = fps

        # determine window size and init wx.Frame
        success, frame = self._acquire_frame()
        if not success:
            print "Could not acquire frame from camera."
            raise SystemExit

        self.imgHeight, self.imgWidth = frame.shape[:2]
        self.bmp = wx.BitmapFromBuffer(self.imgWidth, self.imgHeight, frame)
        wx.Frame.__init__(self, parent, id, title,
                          size=(self.imgWidth, self.imgHeight))

        self._init_base_layout()
        self._create_base_layout()

    def _init_base_layout(self):
        """Initialize parameters

            This method performs initializations that are common to all GUIs,
            such as the setting up of a timer.

            It then calls an abstract method self.init_custom_layout() that
            allows for additional, application-specific initializations.
        """
        # set up periodic screen capture
        self.timer = wx.Timer(self)
        self.timer.Start(1000. / self.fps)
        self.Bind(wx.EVT_TIMER, self._on_next_frame)

        # allow for custom modifications
        self._init_custom_layout()

    def _create_base_layout(self):
        """Create generic layout

            This method sets up a basic layout that is common to all GUIs, such
            as a live stream of the camera (capture device). This stream is
            assigned to the variable self.pnl, and arranged in a vertical
            layout self.panels_vertical.

            Additional layout elements can be added below the livestream by
            means of the method self.panels_vertical.Add.
        """
        # set up video stream
        self.pnl = wx.Panel(self, size=(self.imgWidth, self.imgHeight))
        self.pnl.SetBackgroundColour(wx.BLACK)
        self.pnl.Bind(wx.EVT_PAINT, self._on_paint)

        # display the button layout beneath the video stream
        self.panels_vertical = wx.BoxSizer(wx.VERTICAL)
        self.panels_vertical.Add(self.pnl, 1, flag=wx.EXPAND | wx.TOP,
                                 border=1)

        # allow for custom layout modifications
        self._create_custom_layout()

        # round off the layout by expanding and centering
        self.SetMinSize((self.imgWidth, self.imgHeight))
        self.SetSizer(self.panels_vertical)
        self.Centre()

    def _on_next_frame(self, event):
        """
            This method captures a new frame from the camera (or capture
            device) and sends an RGB version to the method self.process_frame.
            The latter will then apply task-specific post-processing and return
            an image to be displayed.
        """
        success, frame = self._acquire_frame()
        if success:
            # process current frame
            frame = self._process_frame(cv2.cvtColor(frame, cv2.COLOR_BGR2RGB))

            # update buffer and paint (EVT_PAINT triggered by Refresh)
            self.bmp.CopyFromBuffer(frame)
            self.Refresh(eraseBackground=False)

    def _on_paint(self, event):
        """
            This method draws the camera frame stored in the bitmap self.bmp
            onto the panel self.pnl. Make sure self.pnl exists and is at least
            the size of the camera frame.
            This method is called whenever an event wx.EVT_PAINT is triggered.
        """
        # read and draw buffered bitmap
        deviceContext = wx.BufferedPaintDC(self.pnl)
        deviceContext.DrawBitmap(self.bmp, 0, 0)

    def _acquire_frame(self):
        """
            This method is called whenever a new frame needs to be acquired.
            :returns: (success, frame), whether acquiring was successful
                      (via Boolean success) and current frame
        """
        return self.capture.read()

    @abstractmethod
    def _init_custom_layout(self):
        """
            This method is called in the class constructor, after setting up
            relevant event callbacks, and right before creation of the GUI
            layout.
        """
        pass

    @abstractmethod
    def _create_custom_layout(self):
        """
            This method is responsible for creating the GUI layout.
            It is called in the class constructor, after setting up relevant
            event callbacks and self.init_layout, and creates the layout.
            Every GUI contains the camera feed in the variable self.pnl.
            Additional layout elements can be added below the camera feed by
            adding them to self.panels_vertical.
        """
        pass

    @abstractmethod
    def _process_frame(self, frame_rgb):
        """
            This method is responsible for any post-processing that needs to be
            applied to the current frame of the camera (capture device) stream.

            :param frame_rgb: The RGB camera frame to be processed.
            :returns: The processed RGB camera frame to be displayed.
        """
        pass
