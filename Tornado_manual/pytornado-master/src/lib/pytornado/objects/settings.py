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
Data structures for execution settings.

Developed for Airinnova AB, Stockholm, Sweden.
"""

import os
import logging
from pathlib import Path

from pytornado.objects.utils import check_dict, get_default_dict
from commonlibs.fileio.paths import ProjectPaths

logger = logging.getLogger(__name__)

_PLOT_OPTIONS = {
    'opt': ([], list),
    'show': (False, bool),
    'save': (False, bool),
}

_DEFAULT_PLOT_DICT = {
    'geometry': (_PLOT_OPTIONS, dict),
    'lattice': (_PLOT_OPTIONS, dict),
    'matrix_downwash': (_PLOT_OPTIONS, dict),
    'results': (_PLOT_OPTIONS, dict),
}

_DEFAULT_SAVE_DICT = {
    'global': (False, bool),
    'panelwise': (False, bool),
    'aeroperformance': (False, bool),
    'matrix_system': (False, bool),
}

DEFAULT_SETTINGS = {
    'aircraft': (None, str),
    'state': (None, str),
    'deformation': (None, (None, str)),
    'vlm_autopanels_c': (4, int),
    'vlm_autopanels_s': (4, int),
    'save_results': (_DEFAULT_SAVE_DICT, dict),
    'plot': (_DEFAULT_PLOT_DICT, dict),
    # Underscore settings are "hidden" settings that generally shouldn't be changed
    '_do_normal_rotations': (True, bool),
    '_deformation_check': (True, bool),
    '_epsilon': (1e-6, float),
}


class PATHS:
    """
    Namespace for project paths
    """

    class DIR:
        # ----- Input directories -----
        AIRCRAFT = 'aircraft'
        AIRFOILS = 'airfoils'
        DEFORMATION = 'deformation'
        SETTINGS = 'settings'
        STATE = 'state'
        # ----- Output directories -----
        PLOTS = '_plots'
        RESULTS = '_results'
        # ----- Template directory name -----
        TEMPLATE_ROOT = 'pytornado'

    class FILES:
        LOG = 'log.txt'

        @classmethod
        def AIRFOIL(cls, name_airfoil):
            """
            Return relative path to a 'blade' file

            Notes:
                * Path is relative to the project root directory
                * Will return 'airfoils/blade.NACA1234' if 'NACA1234' is given

            Args:
                :name_airfoil: String with the airfoil name
            """

            return os.path.join(PATHS.DIR.AIRFOILS, f"blade.{name_airfoil}")


class Settings:

    def __init__(self, settings_filename, project_dir, *,
                 settings_dict=None, make_dirs=True, check_ac_file_type=True):
        """
        Data structure with PyTornado execution settings

        Attributes:
            :settings_filename: Name of the settings file
            :project_dir: PyTornado project directory (where all data is expected)
            :settings_dict: Basic settings data as a dictionary
            :make_dirs: (bool) Flag for creation of project directories
        """

        self.project_dir = Path(project_dir).resolve()
        self.project_basename = os.path.splitext(settings_filename)[0]
        self.settings = get_default_dict(template_dict=DEFAULT_SETTINGS)

        if settings_dict is not None:
            self.update_from_dict(settings_dict)

        self.paths = None
        self.generate_paths()

        # Aircraft format
        self.aircraft_is_cpacs = None
        if check_ac_file_type:
            self._check_aircraft_file_type()

        if make_dirs:
            self.paths.make_dirs_for_groups('dir')

    @property
    def state_is_cpacs(self):
        """
        Flag indicating if state is to be read from CPACS
        """

        if self.settings['state'].upper() == '__CPACS':
            if not self.aircraft_is_cpacs:
                raise RuntimeError("State cannot be read from CPACS if aircraft file is not CPACS")
            return True
        else:
            return False

    def generate_paths(self):
        """
        Initialise the file structure
        """

        # Output subdirectory to structure results from APM analyses
        output_subdir = f"/{self.project_basename}" + "_{counter:03d}"

        # ===== Directories =====
        self.paths = ProjectPaths(self.project_dir)
        self.paths.add_path(uid='d_aircraft', path=PATHS.DIR.AIRCRAFT, uid_groups='dir')
        self.paths.add_path(uid='d_airfoils', path=PATHS.DIR.AIRFOILS, uid_groups='dir')
        self.paths.add_path(uid='d_deformation', path=PATHS.DIR.DEFORMATION, uid_groups='dir')
        self.paths.add_path(uid='d_settings', path=PATHS.DIR.SETTINGS, uid_groups='dir')
        self.paths.add_path(uid='d_state', path=PATHS.DIR.STATE, uid_groups='dir')
        # Output directories
        self.paths.add_path(uid='d_plots_TOP', path=PATHS.DIR.PLOTS, uid_groups=('dir', 'tmp'))
        self.paths.add_path(uid='d_results_TOP', path=PATHS.DIR.RESULTS, uid_groups=('dir', 'tmp'))
        self.paths.add_path(uid='d_plots', path=PATHS.DIR.PLOTS+output_subdir, uid_groups=('dir', 'tmp'))
        self.paths.add_path(uid='d_results', path=PATHS.DIR.RESULTS+output_subdir, uid_groups=('dir', 'tmp'))

        # ===== Files =====
        self.paths.add_path(uid='f_log', path=PATHS.FILES.LOG)
        self.paths.add_subpath(uid_parent='d_aircraft', uid='f_aircraft', path=f"{self.settings['aircraft']}")
        self.paths.add_subpath(uid_parent='d_deformation', uid='f_deformation', path=f"{self.settings['deformation']}")
        self.paths.add_subpath(uid_parent='d_settings', uid='f_settings', path=f"{self.project_basename}.json")
        self.paths.add_subpath(uid_parent='d_state', uid='f_state', path=f"{self.settings['state']}")
        # Output files
        self.paths.add_subpath(uid_parent='d_results', uid='f_results_global', path=f"global.json")  # File namespaced with folder
        self.paths.add_subpath(uid_parent='d_results', uid='f_results_panelwise', path=f"panelwise.dat")
        self.paths.add_subpath(uid_parent='d_results', uid='f_results_matrix', path=f"matrix.dat")
        self.paths.add_subpath(uid_parent='d_results_TOP', uid='f_results_apm_global', path=f"{self.project_basename}_aeroperformance.json")

    def _check_aircraft_file_type(self):
        """Check whether aircraft is a CPACS or a JSON file"""

        ac_file_extension = self.paths('f_aircraft').suffix.lower()

        if ac_file_extension not in ['.xml', '.json']:
            raise ValueError("Aircraft file must have extension '.json' or '.xml' (CPACS)")

        if ac_file_extension == '.json':
            self.aircraft_is_cpacs = False
        else:
            self.aircraft_is_cpacs = True

    def update_from_dict(self, settings_dict):
        """
        Update settings from dictionary structures

        Args:
            :settings: Dictionary with general settings
            :plot: Dictionary with plot settings
        """

        for key, value in settings_dict.items():
            self.settings[key] = value

        # If 'results' are not specified, plot 'cp' values
        if not self.settings['plot']['results']['opt']:
            self.settings['plot']['results']['opt'] = ['cp']

        self._check_settings_dict()

    def clean(self):
        """
        Remove old files in project directory
        """

        self.paths.rm_dirs_for_groups('tmp')

    def _check_settings_dict(self):
        """
        Check that settings dictionary contains valid input arguments
        """

        logger.debug("Checking settings...")
        check_dict(template_dict=DEFAULT_SETTINGS, test_dict=self.settings)

        # ===== Other checks =====
        if not (0 < self.settings['_epsilon'] < 1):
            raise ValueError("'_epsilon' must be a (small) float in range (0, 1)")
