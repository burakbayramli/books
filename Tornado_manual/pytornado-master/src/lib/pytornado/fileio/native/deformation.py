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
Reading the aircraft deformation file.

Developed at Airinnova AB, Stockholm, Sweden.
"""

import os
import logging

from commonlibs.logger import truncate_filepath
from aeroframe.fileio.serialise import load_json_def_fields

logger = logging.getLogger(__name__)


def load(aircraft, settings):
    """
    Loads the aircraft deformation file if it exitsts.

    Args:
        :aircraft: (obj) aircraft
        :settings: (obj) settings
    """

    filepath = settings.paths('f_deformation')
    logger.info(f"Reading deformation from file '{truncate_filepath(filepath)}'")

    if not os.path.exists(filepath):
        raise IOError(f"file '{filepath}' not found")
    # File is empty or as good as (this also catches empty JSON file: '{}')
    elif os.stat(filepath).st_size < 10:
        logger.warning(f"Empty deformation file. No deformations are modelled.")
        return

    def_fields = load_json_def_fields(filepath)
    for wing_uid, def_field in def_fields.items():
        ####
        # Convention: Deformation fields starting with '_' will be ignore by CFD
        if wing_uid.startswith('_'):
            continue
        ####

        # Convention: Deformation fields ending with '_m' belongs to a 'mirrored' component
        if wing_uid.endswith('_m'):
            aircraft.wings[wing_uid.replace('_m', '')].def_field_mirror = def_field
            continue

        aircraft.wings[wing_uid].def_field = def_field

    # TODO: Handle exceptions
    # TODO: Check deformation continuity
