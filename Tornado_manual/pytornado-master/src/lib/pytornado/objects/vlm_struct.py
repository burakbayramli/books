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
Data structures for VLM-related data: grid and results

Developed for Airinnova AB, Stockholm, Sweden.
"""

from collections import defaultdict, namedtuple


BookKeepingEntry = namedtuple(
    'BookKeepingEntry',
    ['subarea', 'pan_idx', 'num_chordwise_panels', 'mirror']
)


class VLMLattice:

    def __init__(self):
        """
        Data structure for the VLM lattice

        Note:
            * VLMLattice contains the lattice data required for VLM analysis
            * The data is stored in pre-allocated, contiguous-memory C-arrays

        Attributes:
            :p: (numpy) panel corner points (N x 4 x 3)
            :v: (numpy) panel vortex points (N x 4 x 3)
            :c: (numpy) panel collocation points (N x 3)
            :bound_leg_midpoints: (numpy) midpoint of the bound leg (N x 3)
            :n: (numpy) panel normal vectors (N x 3)
            :a: (numpy) panel surface areas (N x 1)
            :info: (dict) lattice statistics (quantity, quality)
            :epsilon: (float) Small number used to avoid division by 0
            :panel_bookkeeping: (list) List with bookkeeping entries
            :bookkeeping_by_wing_uid: (dict) BookKeeping entries ordered by wing UID
            :bookkeeping_by_wing_uid_mirror: (dict) BookKeeping entries of mirror ordered by wing UID
        """

        self.p = None
        self.v = None
        self.c = None
        self.n = None
        self.a = None
        self.bound_leg_midpoints = None

        self.info = {}
        self.info['num_wings'] = 0
        self.info['num_segments'] = 0
        self.info['num_controls'] = 0
        self.info['num_panels'] = 0
        self.info['num_strips'] = 0
        self.info['aspect_min'] = 0.0
        self.info['aspect_max'] = 0.0
        self.info['aspect_avg'] = 0.0
        self.info['area_min'] = 0.0
        self.info['area_max'] = 0.0
        self.info['area_avg'] = 0.0

        self.epsilon = None

        self.panel_bookkeeping = []
        self.bookkeeping_by_wing_uid = defaultdict(list)
        self.bookkeeping_by_wing_uid_mirror = defaultdict(list)

    def update_bookkeeping(self, entry):
        """
        Add a 'BookKeepingEntry()' to the bookkeeping system

        Args:
            :entry: (obj) Instance of 'BookKeepingEntry()'
        """

        self.panel_bookkeeping.append(entry)
        if entry.mirror:
            self.bookkeeping_by_wing_uid_mirror[entry.subarea.parent_wing.uid].append(entry)
        else:
            self.bookkeeping_by_wing_uid[entry.subarea.parent_wing.uid].append(entry)

    def clean_bookkeeping(self):
        """
        Remove all bookkeeping entries
        """

        self.panel_bookkeeping = []
        self.bookkeeping_by_wing_uid = defaultdict(list)
        self.bookkeeping_by_wing_uid_mirror = defaultdict(list)


class VLMData:

    def __init__(self):
        """
        Data structure for the VLM analysis results

        Note:
            * VLMData contains the data produced during VLM analysis
            * The data is stored in pre-allocated, contiguous-memory C-arrays

        Attributes:
            :panelwise: (dict) dictionary of panel-wise properties (NP)
            :stripwise: (dict) dictionary of strip-wise properties (NS)
            :forces: (dict) dictionary of aero forces (1)
            :coeffs: (dict) dictionary of aero coefficients (1)
            :matrix_downwash: (TODO)
            :array_rhs: (TODO)
            :matrix_lu: (TODO)
            :array_pivots: (TODO)
        """

        self.matrix_downwash = None
        self.array_rhs = None

        self.matrix_lu = None
        self.array_pivots = None

        # On unit of panelwise results:
        # Circulation [gamma] = m²/s
        # Velocity v{x,y,z} [m/s]
        # Panel forces f{x,y,z} [Newton] (NOT force per length)
        self.panelwise = {}
        self.panelwise['gamma'] = None
        self.panelwise['vx'] = None
        self.panelwise['vy'] = None
        self.panelwise['vz'] = None
        self.panelwise['vmag'] = None
        self.panelwise['fx'] = None
        self.panelwise['fy'] = None
        self.panelwise['fz'] = None
        self.panelwise['fmag'] = None
        self.panelwise['cp'] = None

        self.stripwise = {}
        self.stripwise['cl'] = None

        self.forces = {}
        self.forces['x'] = 0.0
        self.forces['y'] = 0.0
        self.forces['z'] = 0.0
        self.forces['D'] = 0.0
        self.forces['C'] = 0.0
        self.forces['L'] = 0.0
        self.forces['l'] = 0.0
        self.forces['m'] = 0.0
        self.forces['n'] = 0.0

        self.coeffs = {}
        self.coeffs['x'] = 0.0
        self.coeffs['y'] = 0.0
        self.coeffs['z'] = 0.0
        self.coeffs['D'] = 0.0
        self.coeffs['C'] = 0.0
        self.coeffs['L'] = 0.0
        self.coeffs['l'] = 0.0
        self.coeffs['m'] = 0.0
        self.coeffs['n'] = 0.0
