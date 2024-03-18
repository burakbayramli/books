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
Data structure for aircraft flight state.

Developed for Airinnova AB, Stockholm, Sweden.
"""


import logging

import numpy as np
from numpy import cos, sin, deg2rad
from ambiance import Atmosphere


logger = logging.getLogger(__name__)


AIRSPEED = 'airspeed'
ALPHA = 'alpha'
ALTITUDE = 'altitude'
BETA = 'beta'
DENSITY = 'density'
MACH = 'mach'
RATE_P = 'rate_P'
RATE_Q = 'rate_Q'
RATE_R = 'rate_R'

STATE_ALL_PARAMS = [
    AIRSPEED,
    ALPHA,
    ALTITUDE,
    BETA,
    DENSITY,
    MACH,
    RATE_P,
    RATE_Q,
    RATE_R,
]

# Fundamental parameters used to define a flight state for a VLM calculation
STATE_PRIMARY_PARAMS = [
    AIRSPEED,
    ALPHA,
    BETA,
    DENSITY,
    RATE_P,
    RATE_Q,
    RATE_R,
]

# Alternative input parameters
STATE_SECONDARY_PARAMS = [
    ALTITUDE,
    MACH,
]

GLOBAL_COEFFS = [
    'Fx', 'Fy', 'Fz',
    'FD', 'FC', 'FL',
    'Mx', 'My', 'Mz',
    'Cx', 'Cy', 'Cz',
    'CD', 'CC', 'CL',
    'Cl', 'Cm', 'Cn',
]


class CurrentState:

    def __init__(self):
        """
        Class reflecting the currently analysed state

        Attributes:
            :aero: (dict) see FlightState()
            :refs: (dict) see model.Aircraft()
            :free_stream_velocity_vector: (vector)  free stream vector
        """

        self.aero = None
        self.refs = None
        self.free_stream_velocity_vector = None


class FlightState:

    def __init__(self):
        """
        Data structure for the flight conditions

        Attributes:
            :aero: (dict) aerodynamic operating parameters
            :results: (dict) aeroperformance map results
            :num_apm_entries: (int) Number of values is APM
            :idx_current_state: (int) Index of the current flight state

        Note:
            * The angles 'alpha' and 'beta' are assumed to be given in DEGREES!
              Alpha and beta are user input. The angles stored in this state class
              will not be changed during runtime.
        """

        self.aero = {}
        for prop in STATE_ALL_PARAMS:
            self.aero[prop] = None

        self.results = {}
        for param in GLOBAL_COEFFS:
            self.results[param] = []

        # Number of values in the aeroperformance map
        self.num_apm_entries = 0
        self.idx_current_state = None

    @property
    def free_stream_velocity_vector(self):
        """Return the free stream velocity vector (incoming flow)"""

        i = self.idx_current_state
        alpha = deg2rad(self.aero[ALPHA][i])
        beta = deg2rad(self.aero[BETA][i])
        airspeed = self.aero[AIRSPEED][i]

        if None in [alpha, beta, airspeed]:
            raise RuntimeError("'alpha', 'beta' and 'airspeed' must be set")

        free_stream_vel = airspeed*np.array([cos(alpha)*cos(beta),
                                             -sin(beta),
                                             sin(alpha)*cos(beta)])
        return free_stream_vel

    def update_from_dict(self, aero):
        """
        Update state using a dictionary
        """

        # ----- Make everything into lists -----
        array_lenghts = set()
        for key, value in aero.items():
            if not isinstance(value, list):
                value = [value]

            array_lenghts.add(len(value))
            self.aero[key] = value

        if len(array_lenghts) > 1:
            raise ValueError("State arrays have different lenghts!")

        self.num_apm_entries = int(list(array_lenghts)[0])

        # ----- We can allow different input combinations -----
        all_props = {AIRSPEED, MACH, DENSITY, ALTITUDE}
        valid_prop_pairs = [
            {AIRSPEED, DENSITY},
            {AIRSPEED, ALTITUDE},
            {MACH, ALTITUDE},
        ]

        is_input = {prop: False for prop in all_props}
        for prop in all_props:
            if self.aero[prop] is not None:
                if all(isinstance(value, (int, float)) for value in self.aero[prop]):
                    is_input[prop] = True

        input_props = set(prop for prop in all_props if is_input[prop] is True)
        if input_props not in valid_prop_pairs:
            err_msg = """
            Invalid combination: You may set:
            (1) 'airspeed' and 'density' or
            (2) 'airspeed' and 'altitude' or
            (3) 'mach' and 'altitude'
            Make sure the remaining values are all set to 'None'.
            """
            raise ValueError(err_msg)

        # ----- Only numpy float arrays from now on -----
        for prop in STATE_ALL_PARAMS:
            if self.aero[prop] is not None:
                self.aero[prop] = np.array(self.aero[prop], dtype=float)

        # ----- Compute airspeed and density if necessary -----
        if is_input[ALTITUDE]:
            atmosphere = Atmosphere(self.aero[ALTITUDE])
            density = np.array(atmosphere.density)
            speed_of_sound = np.array(atmosphere.speed_of_sound)

        if not is_input[DENSITY]:
            self.aero[DENSITY] = density

        if is_input[MACH]:
            self.aero[AIRSPEED] = self.aero[MACH]*speed_of_sound

        self.check_values()

    def check_values(self):
        """Make sure input values have correct format"""

        # Check that angles are in correct range
        for angle in [ALPHA, BETA]:
            range_is_not_ok = (self.aero[angle] < -90) & (self.aero[angle] > 90)
            if any(range_is_not_ok):
                raise ValueError(f"Angle '{angle}' must be in range [-90, 90] degrees")

    def iter_states(self):
        """
        Iterator which yields a dictionary for each flight state
        """

        for i in range(0, self.num_apm_entries):
            self.idx_current_state = i

            current_state = CurrentState()

            current_aero = {}
            for param in STATE_PRIMARY_PARAMS:
                current_aero[param] = float(self.aero[param][i])

            current_state.aero = current_aero
            current_state.free_stream_velocity_vector = self.free_stream_velocity_vector
            yield current_state
