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
Generate project directory folder structure with input file templates.

Developed at Airinnova AB, Stockholm, Sweden.
"""


from pathlib import Path
import os
import shutil
import sys

from pytornado.objects.aircraft import Aircraft
from pytornado.objects.settings import Settings, PATHS
from pytornado.objects.state import FlightState
import pytornado.fileio as io


def setup_wkdir(aircraft_file=None):
    """
    Create a template working directory with a minimal working example

    Notes:
        * The project directory contains all data related to the aircraft model
        * This includes flight state definitions and execution settings
        * The native format is used

    Args:
        :aircraft_file: (str) Full aircraft file path
    """

    # We create a separate directory for the template data
    # to avoid potential cluttering the user's directory
    project_dir = os.path.abspath(PATHS.DIR.TEMPLATE_ROOT)

    if os.path.exists(project_dir):
        err_msg = f"""
        The path '{os.path.basename(project_dir)}' does already exist. Refusing to proceed.
        Please move or delete the folder, then try again.
        """
        print(err_msg, file=sys.stderr)
        sys.exit(1)
    else:
        print(f"Creating template in folder '{os.path.basename(project_dir)}'...")
        os.makedirs(project_dir)

    if aircraft_file is None:
        project_basename = "template"
        settings_filename = project_basename + ".json"
    else:
        project_basename = Path(aircraft_file).stem
        settings_filename = Path(aircraft_file).name

    # Generate file names
    settings = Settings(settings_filename, project_dir, check_ac_file_type=False)

    if aircraft_file is not None:
        # ---------- Settings ----------
        settings.settings['aircraft'] = project_basename + '.json'
        settings.settings['state'] = project_basename + '.json'
        settings.generate_paths()

        # Copy the aircraft file into the aircraft directory
        shutil.copy(aircraft_file, settings.paths('d_aircraft'))
        aircraft = io.native.aircraft.load(settings)
    # If no aircraft file specified, we generate a simple rectangular wing
    else:
        # ---------- Aircraft ----------
        aircraft = Aircraft()
        aircraft.uid = 'template_aircraft'

        aircraft.refs['area'] = 10
        aircraft.refs['span'] = 5
        aircraft.refs['chord'] = 2
        aircraft.refs['gcenter'] = [0, 0, 0]
        aircraft.refs['rcenter'] = [0, 0, 0]

        wing = aircraft.add_wing('template_wing')
        segment = wing.add_segment('template_segment')
        control = wing.add_control('template_control')

        # ---------- Wing ----------
        wing.symmetry = 2

        # ---------- Segment ----------
        segment.vertices['a'] = [0, 0, 0]
        segment.vertices['b'] = [0, 5, 0]
        segment.vertices['c'] = [2, 5, 0]
        segment.vertices['d'] = [2, 0, 0]
        segment.airfoils['inner'] = 'NACA0000'
        segment.airfoils['outer'] = 'NACA0000'

        # ---------- Control ----------
        control.device_type = 'flap'
        control.deflection = 5
        control.deflection_mirror = -5
        control.segment_uid['inner'] = 'template_segment'
        control.segment_uid['outer'] = 'template_segment'
        control.rel_vertices['eta_inner'] = 0.2
        control.rel_vertices['eta_outer'] = 0.8
        control.rel_vertices['xsi_inner'] = 0.7
        control.rel_vertices['xsi_outer'] = 0.7
        control.rel_hinge_vertices['xsi_inner'] = 0.7
        control.rel_hinge_vertices['xsi_outer'] = 0.7

        # ---------- Settings ----------
        settings.settings['aircraft'] = aircraft.uid + '.json'
        settings.settings['state'] = project_basename + '.json'
        settings.generate_paths()

    settings.settings['vlm_autopanels_s'] = 20
    settings.settings['vlm_autopanels_c'] = 5
    settings.settings['plot']['results']['show'] = True
    settings.settings['plot']['results']['opt'] = ['cp']

    # ---------- State ----------
    state = FlightState()
    state.aero['airspeed'] = 100
    state.aero['density'] = 1.225
    state.aero['alpha'] = 2
    state.aero['beta'] = 0
    state.aero['rate_P'] = 0
    state.aero['rate_Q'] = 0
    state.aero['rate_R'] = 0

    # Save settings, state and model file
    io.native.settings.save(settings)
    io.native.state.save(state, settings)
    io.native.aircraft.save(aircraft, settings)
    return project_dir


def cpacs2pytornado(file_cpacs):
    """
    Load a CPACS file and export JSON Aircraft and state files

    Args:
        :file_cpacs: (string) absolute path to project directory
    """

    settings = Settings(
        settings_filename='',
        project_dir=os.getcwd(),
        make_dirs=False,
        check_ac_file_type=False
    )
    settings.paths.remove_path('f_aircraft')
    settings.paths.add_path(path=os.path.abspath(file_cpacs), uid='f_aircraft', is_absolute=True)

    # Get the aircraft object
    aircraft = io.cpacs.aircraft.load(settings)

    # Write JSON file
    settings.paths.change_suffix(uid='f_aircraft', new_suffix='.json')
    io.native.aircraft.save(aircraft, settings)
