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
# * Aaron Dettmann

"""
General CPACS functions
"""

from contextlib import contextmanager

import logging

logger = logging.getLogger(__name__)

try:
    import tixi3.tixi3wrapper as tixiwrapper
    tixiwrapper.Tixi = tixiwrapper.Tixi3
    tixiwrapper.TixiException = tixiwrapper.Tixi3Exception
except ImportError:
    TIXI_INSTALLED = False
else:
    TIXI_INSTALLED = True

try:
    import tigl3.tigl3wrapper as tiglwrapper
    tiglwrapper.Tigl = tiglwrapper.Tigl3
except ImportError:
    TIGL_INSTALLED = False
else:
    TIGL_INSTALLED = True


class _XPaths:
    """
    Namespace for CPACS paths
    """

    # General CPACS paths
    MODEL = '/cpacs/vehicles/aircraft/model'
    WINGS = MODEL + '/wings'
    REFS = MODEL + '/reference'
    AIRFOILS = '/cpacs/vehicles/profiles/wingAirfoils'
    APMAP = MODEL + '/analyses/aeroPerformance'

    # Tool specific
    TOOLSPEC = '/cpacs/toolspecific/pytornado'
    TOOLSPEC_CONTROL = TOOLSPEC + '/controlDevices'

    @classmethod
    def CONTROL(cls, idx_wing, idx_comp_section, idx_control, device_pos):
        control = cls.WINGS \
            + '/wing[{0:d}]/componentSegments/componentSegment[{1:d}]' \
            + '/controlSurfaces/{3:s}EdgeDevices/{3:s}EdgeDevice[{2:d}]'
        return control.format(idx_wing, idx_comp_section, idx_control, device_pos)

    @classmethod
    def APM(cls, tixi):
        """
        Return the AeroPerformanceMap for a given UID

        Args:
            :tixi: Tixi handle
            :uid_apm: AeroPerformanceMap UID
        """

        uid_apm = cls._get_uid_apm(tixi)
        xpath_apm = tixi.uIDGetXPath(uid_apm)
        xpath_apm += '/aeroPerformanceMap'
        return xpath_apm

    @classmethod
    def _get_uid_apm(cls, tixi):
        """
        Return the UID of the aeroperformance map to import

        Notes:
            * If there is only a single aeroperformance map, we use that one
            * If there are multiple maps, a UID must be provided

        Args:
            :tixi: Tixi handle

        Returns:
            :uid_apm: UID of the aeroperformance map
        """

        num_apm = tixi.getNumberOfChilds(cls.APMAP)
        if num_apm == 1:
            uid_apm = tixi.getTextAttribute(cls.APMAP + '/aeroMap', 'uID')
        else:
            uid_apm = tixi.getTextElement(cls.TOOLSPEC + '/aeroMapUID')

        return uid_apm


XPATHS = _XPaths


def open_tixi(cpacs_file):
    """
    Return a Tixi handle

    Args:
        :cpacs_file: CPACS file path

    Returns:
        :tixi: Tixi handle
    """

    logger.debug("Checking Tixi installation...")
    if not TIXI_INSTALLED:
        err_msg = """
        Unable to import Tixi. Please make sure Tixi is accessible to Python.
        Please refer to the documentation to check supported versions of Tixi.
        """
        logger.error(err_msg)
        raise ModuleNotFoundError(err_msg)

    # Note: Casting of filepath to string is necessary, because Tixi only
    # handles string (not Path() objects)
    tixi = tixiwrapper.Tixi()
    tixi.open(str(cpacs_file))
    return tixi


def close_tixi(tixi, cpacs_file):
    """
    Close Tixi and save to file

    Args:
        :tixi: Tixi handle
        :cpacs_file: CPACS file path
    """

    tixi.saveDocument(str(cpacs_file))
    tixi.close()


def open_tigl(tixi):
    """
    Return a Tigl handle

    Args:
        :tixi: Tixi handle

    Returns:
        :tigl: Tigl handle
    """

    logger.debug("Checking Tigl installation...")
    if not TIGL_INSTALLED:
        err_msg = """
        Unable to import Tigl. Please make sure Tigl is accessible to Python.
        Please refer to the documentation to check supported versions of Tigl.
        """
        logger.error(err_msg)
        raise ModuleNotFoundError(err_msg)

    tigl = tiglwrapper.Tigl()
    # On argument 'uid' from Tigl documentation: The UID of the configuration
    # that should be loaded by TIGL. Could be NULL or an empty string if the
    # data set contains only one configuration.
    tigl.open(tixi, uid='')
    return tigl


def get_segment_mid_point(tigl, idx_wing, idx_segment, eta, xsi):
    """
    Return a mid point for a segment

    Args:
        :tigl: Tigl handle
        :idx_wing: Wing index
        :idx_segment: Segment index
        :eta: Relative segment coordinate
        :xsi: Relative segment coordinate

    Returns:
        :mid_point: List with mid coordinate
    """

    lower = tigl.wingGetLowerPoint(idx_wing, idx_segment, eta, xsi)
    upper = tigl.wingGetUpperPoint(idx_wing, idx_segment, eta, xsi)
    mid_point = [(l + u)/2.0 for l, u in zip(lower, upper)]
    return mid_point


def add_vector(tixi, xpath, vector):
    """
    Add a vector at given CPACS path

    Note:
        * Values will be overwritten if paths exists

    Args:
        :tixi: Tixi handle
        :xpath: CPACS path
        :vector: List or tuple (vector) to add
    """

    # Strip trailing '/' (has no meaning here)
    if xpath.endswith("/"):
        xpath = xpath[:-1]

    # Get the field name and the parent CPACS path
    xpath_child_name = xpath.split("/")[-1]
    xpath_parent = xpath[:-(len(xpath_child_name)+1)]

    if tixi.checkElement(xpath):
        tixi.updateFloatVector(xpath, vector, len(vector), format='%g')
    else:
        tixi.addFloatVector(xpath_parent, xpath_child_name, vector, len(vector), format='%g')


@contextmanager
def modify_cpacs(cpacs_file):
    """
    Context manganger for handling CPACS files

    Args:
        :cpacs_file: CPACS file path
    """

    tixi = open_tixi(cpacs_file)
    try:
        yield tixi
    finally:
        close_tixi(tixi, cpacs_file)
