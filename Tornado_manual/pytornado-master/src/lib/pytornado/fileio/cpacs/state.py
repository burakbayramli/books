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
Import of CPACS aeroperformance maps (flight states) and export of results

Developed at Airinnova AB, Stockholm, Sweden.
"""

import logging

from pytornado.fileio.cpacs.utils import open_tixi, XPATHS
from pytornado.objects.state import FlightState, ALTITUDE, ALPHA, BETA, MACH, RATE_P, RATE_Q, RATE_R

logger = logging.getLogger(__name__)


def load(settings):
    """
    Load the flight state from the CPACS aeroperformance map
    """

    cpacs_file = settings.paths('f_aircraft')
    logger.info(f"Loading state from CPACS file: {cpacs_file}...")
    if not cpacs_file.is_file():
        err_msg = f"File '{cpacs_file}' not found or not valid file"
        logger.error(err_msg)
        raise FileNotFoundError(err_msg)

    tixi = open_tixi(cpacs_file)

    state_dict = _get_aero_dict_from_APM(tixi)
    state = FlightState()
    state.update_from_dict(**state_dict)
    return state


def _get_aero_dict_from_APM(tixi):
    """
    Extract the aeroperformance map from CPACS and return an aero dictionary

    Args:
        :tixi: Tixi handle

    Returns:
        :state_dict: state dictionary
    """

    state_dict = {
        "aero": {
            ALTITUDE: None,
            ALPHA: None,
            BETA: None,
            MACH: None,
            RATE_P: None,
            RATE_Q: None,
            RATE_R: None,
        }
    }

    # Map the CPACS state names to the PyTornado STATE NAMES
    state_params = {
        'altitude': ALTITUDE,
        'machNumber': MACH,
        'angleOfAttack': ALPHA,
        'angleOfSideslip': BETA,
    }

    vector_lens = set()
    xpath_apm = XPATHS.APM(tixi)
    for cpacs_key, state_key in state_params.items():
        xpath_apm_param = xpath_apm + '/' + cpacs_key
        vector_len = tixi.getVectorSize(xpath_apm_param)
        vector_lens.add(vector_len)
        values = tixi.getFloatVector(xpath_apm_param, vector_len)
        state_dict['aero'][state_key] = list(values)

    if len(list(vector_lens)) > 1:
        err_msg = f"""
        CPACS AeroPerformanceMap contains vectors of different length.
        All vectors must have the same number of elements.
        """
        raise ValueError(err_msg)

    # NOTE: Roll rate are not an input parameter in CPACS
    for rate in [RATE_P, RATE_Q, RATE_R]:
        state_dict['aero'][rate] = [0]*vector_len

    return state_dict
