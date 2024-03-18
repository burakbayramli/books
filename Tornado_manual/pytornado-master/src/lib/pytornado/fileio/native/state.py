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
Functions for reading and writing of PyTornado flight state definition.

Developed at Airinnova AB, Stockholm, Sweden.
"""

import os
import logging
import json

from commonlibs.logger import truncate_filepath

from pytornado.fileio.utils import dump_pretty_json
from pytornado.objects.state import FlightState

logger = logging.getLogger(__name__)


def load(settings):
    """
    Read flight state from PyTornado state file.

    Searches for file 'state.*' in the 'state' folder of the WKDIR.

    Args:
        :state: (object) data structure for operating conditions
        :settings: (object) data structure for execution settings
    """

    state_file = settings.paths('f_state')
    logger.info(f"Reading flight state from file '{truncate_filepath(state_file)}'...")

    if not os.path.exists(state_file):
        raise IOError(f"File '{state_file}' not found")

    with open(state_file, 'r') as fp:
        state_dict = json.load(fp)

    state = FlightState()
    state.update_from_dict(**state_dict)
    return state


def save(state, settings):
    """
    Write flight state to PyTornado state file.

    Args:
        :state: (object) data structure for operating conditions
        :settings: (object) data structure for execution settings
    """

    state_file = settings.paths('f_state')
    logger.info(f"Writing flight state to file '{truncate_filepath(state_file)}'")

    output = {}

    for key in ['aero']:
        output[key] = dict(getattr(state, key))

    with open(state_file, 'w') as fp:
        dump_pretty_json(output, fp)
