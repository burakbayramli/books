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
Functions for reading and writing of PyTornado aircraft definition file.

Developed at Airinnova AB, Stockholm, Sweden.
"""

import os
import logging
import json

from commonlibs.logger import truncate_filepath

from pytornado.fileio.utils import dump_pretty_json
from pytornado.objects.aircraft import Aircraft
from pytornado.objects.settings import PATHS

logger = logging.getLogger(__name__)


def load(settings):
    """
    Read aircraft model from PyTornado aircraft definition file.

    Searches for file 'aircraft.*' in the 'aircraft' folder of the WKDIR.

    Args:
        :settings: (object) data structure for execution settings

    Returns:
        :aircraft: (object) data structure for aircraft model
    """

    aircraft_file = settings.paths('f_aircraft')
    logger.info(f"Reading aircraft model from file '{truncate_filepath(aircraft_file)}'...")

    if not os.path.exists(aircraft_file):
        raise IOError(f"file '{aircraft_file}' not found")

    with open(aircraft_file, 'r') as fp:
        aircraft_dict = json.load(fp)

    # ====== Aircraft top level =====
    aircraft = Aircraft()
    aircraft.uid = aircraft_dict['uid']

    for key, value in aircraft_dict['refs'].items():
        aircraft.refs[key] = value

    # ====== Wings =====
    for wing_entry in aircraft_dict['wings']:
        wing = aircraft.add_wing(wing_entry['uid'])
        wing.symmetry = wing_entry['symmetry']

        # ====== Segments =====
        for segment_entry in wing_entry['segments']:
            segment = wing.add_segment(segment_entry['uid'])

            for key, value in segment_entry['vertices'].items():
                segment.vertices[key] = value

            if segment_entry.get('geometry', None):
                for key, value in segment_entry['geometry'].items():
                    segment.geometry[key] = value

            for key, value in segment_entry['airfoils'].items():
                # From now on use the absolute file path
                if value.startswith(PATHS.DIR.AIRFOILS):
                    value = os.path.join(settings.paths('root'), value)
                segment.airfoils[key] = value

            if segment_entry.get('panels', None):
                for key, value in segment_entry['panels'].items():
                    segment.panels[key] = value

        # ====== Controls =====
        for control_entry in wing_entry.get('controls', []):
            control = wing.add_control(control_entry['uid'])
            control.device_type = control_entry['device_type']
            control.deflection = control_entry['deflection']
            control.deflection_mirror = control_entry.get('deflection_mirror', None)

            for key, value in control_entry['segment_uid'].items():
                control.segment_uid[key] = value

            for key, value in control_entry['rel_vertices'].items():
                control.rel_vertices[key] = value

            for key, value in control_entry['rel_hinge_vertices'].items():
                control.rel_hinge_vertices[key] = value

            control.panels['num_c'] = control_entry['panels']['num_c']
            control.check()

    aircraft.generate()
    return aircraft


def save(aircraft, settings):
    """
    Retrieve data from aircraft model and serialise

    Args:
        :aircraft: (object) data structure for aircraft
        :settings: (object) data structure for execution settings
    """

    filepath = settings.paths('f_aircraft')
    logger.info(f"Writing aircraft model to file '{truncate_filepath(filepath)}'...")

    # ====== Aircraft top level =====
    output = {}
    output['uid'] = aircraft.uid

    output['refs'] = {}
    for key, value in aircraft.refs.items():
        output['refs'][key] = value

    # ====== Wings =====
    output['wings'] = []
    for wing in aircraft.wings.values():
        wing_entry = {}
        wing_entry['uid'] = wing.uid
        wing_entry['symmetry'] = wing.symmetry

        # ====== Segments =====
        wing_entry['segments'] = []
        for segment in wing.segments.values():
            segment_entry = {}
            segment_entry['uid'] = segment.uid
            segment_entry['vertices'] = dict(segment.vertices)

            segment_entry['geometry'] = {}
            for key, value in segment.geometry.items():
                segment_entry['geometry'][key] = value

            segment_entry['airfoils'] = {}
            for key, value in segment.airfoils.items():
                # If airfoil is "blade" file, make sure to save as relative path
                # Note: Airfoil definition may also be for instance "NACA1234"
                if "blade." in value:
                    # Make path relative!
                    value = os.path.join(PATHS.DIR.AIRFOILS, os.path.basename(value))
                segment_entry['airfoils'][key] = value

            segment_entry['panels'] = {}
            for key, value in segment.panels.items():
                segment_entry['panels'][key] = value

            wing_entry['segments'].append(segment_entry)

        # ====== Controls =====
        wing_entry['controls'] = []
        for control in wing.controls.values():
            control_entry = {}

            control_entry['uid'] = control.uid
            control_entry['device_type'] = control.device_type
            control_entry['deflection'] = control.deflection
            control_entry['deflection_mirror'] = control.deflection_mirror

            control_entry['segment_uid'] = {}
            for key, value in control.segment_uid.items():
                control_entry['segment_uid'][key] = value

            control_entry['rel_vertices'] = {}
            for key, value in control.rel_vertices.items():
                control_entry['rel_vertices'][key] = value

            control_entry['rel_hinge_vertices'] = {}
            for key, value in control.rel_hinge_vertices.items():
                control_entry['rel_hinge_vertices'][key] = value

            control_entry['panels'] = {}
            for key, value in control.panels.items():
                control_entry['panels'][key] = value

            wing_entry['controls'].append(control_entry)

        output['wings'].append(wing_entry)

    with open(filepath, 'w') as fp:
        dump_pretty_json(output, fp)
