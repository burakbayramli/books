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
Write VLM results back to CPACS

Developed at Airinnova AB, Stockholm, Sweden.
"""

import logging

from commonlibs.logger import truncate_filepath

from pytornado.fileio.cpacs.utils import XPATHS, add_vector, modify_cpacs

logger = logging.getLogger(__name__)


def save_aeroperformance_map(state, settings):
    """
    Write aeroperformance map results back to CPACS

    Args:
        :state: (object) data structure for operating conditions
        :settings: (object) data structure for execution settings
    """

    state.results

    cpacs_file = settings.paths('f_aircraft')
    logger.info(f"Writing aeroperformance map results to '{truncate_filepath(cpacs_file)}'")

    cpacs_mappings = {
        ('cl', 'CL'),  # Lift coefficient in aerodynamic coordinates
        ('cd', 'CD'),  # Drag coefficient in aerodynamic coordinates
        ('cs', 'CC'),  # Coefficient of the side force vector in aerodynamic coordinates
        ('cmd', 'Cl'),  # Moment coefficient about x
        ('cms', 'Cm'),  # Moment coefficient about y
        ('cml', 'Cn'),  # Moment coefficient about z
    }

    with modify_cpacs(cpacs_file) as tixi:
        for key_cpacs, key_native in cpacs_mappings:
            xpath = XPATHS.APM(tixi) + "/" + key_cpacs
            add_vector(tixi, xpath, vector=state.results[key_native])
