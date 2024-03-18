#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# ----------------------------------------------------------------------
# Copyright 2017-2020 Airinnova AB and the PyTornado authors
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# ----------------------------------------------------------------------

# Authors:
# * Alessandro Gastaldi
# * Aaron Dettmann

"""
Visualisation of the VLM downwash matrix

Developed at Airinnova AB, Stockholm, Sweden.
"""


import logging

import numpy as np
import matplotlib.pyplot as plt

import pytornado.plot.plottools as pt

logger = logging.getLogger(__name__)


def view_downwash(vlmdata, plt_settings):
    """
    Visualise matrix of downwash factors

    Args:
        :vlmdata: (object) data structure for VLM analysis data
        :plt_settings: general plot settings
    """

    logger.info("Generating downwash plot...")

    if not isinstance(vlmdata.matrix_downwash, np.ndarray):
        err_msg = "Downwash factor matrix is not a numpy array"
        logger.error(err_msg)
        raise TypeError(err_msg)

    figure = plt.figure(figsize=(9, 9))
    axes = figure.add_subplot(111)
    axes.set_aspect('equal')
    axes.matshow(vlmdata.matrix_downwash, cmap=pt.C.COLORMAP)
    axes.set_xlabel('i')
    axes.set_ylabel('j')
    axes.set_title("Downwash factor matrix")

    pt.show_and_save(plt_settings, (figure, 'downwash'))
    plt.close('all')
