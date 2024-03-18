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
Functions for writing of PyTornado VLM results.

Developed at Airinnova AB, Stockholm, Sweden.
"""

import logging

import numpy as np
from commonlibs.logger import truncate_filepath

from pytornado.fileio.utils import dump_pretty_json

logger = logging.getLogger(__name__)

FMT_NUM = '%13.6e'

# Hints:
# * Large data sets should not be saved too JSON (too slow)
# * Using Numpy's savetxt() function is preferable


def save_all(settings, aircraft, state, vlmdata):
    """
    Evaluate all 'save' settings and create result files

    Args:
        :settings: settings instance
        :aircraft: (object) data structure for aircraft model
        :state: (object) data structure for operating conditions
        :vlmdata: (object) data structure for VLM calculation data
        :lattice: (object) data structure for VLM lattice
    """

    if settings.settings['save_results']['global']:
        _save_glob_results(state, vlmdata, settings)

    if settings.settings['save_results']['panelwise']:
        _save_panelwise(state, vlmdata, settings)

    if settings.settings['save_results']['matrix_system']:
        _save_matrix_system(state, vlmdata, settings)


def _save_glob_results(state, vlmdata, settings):
    """
    Save global results

    Args:
        :state: (object) data structure for operating conditions
        :vlmdata: (object) data structure for VLM calculation data
        :settings: (object) data structure for execution settings
    """

    filepath = settings.paths('f_results_global')
    logger.info(f"Writing global results to file '{truncate_filepath(filepath)}'")

    output = {}
    output['aero'] = {k: v for k, v in state.aero.items()}
    output['refs'] = {k: v for k, v in state.refs.items()}
    output['global_forces'] = {k: v for k, v in vlmdata.forces.items()}
    output['global_coeffs'] = {k: v for k, v in vlmdata.coeffs.items()}

    with open(filepath, "w") as fp:
        dump_pretty_json(output, fp)


def _save_panelwise(state, vlmdata, settings):
    """
    Save panelwise results

    Args:
        :state: (object) data structure for operating conditions
        :vlmdata: (object) data structure for VLM calculation data
        :settings: (object) data structure for execution settings
    """

    filepath = settings.paths('f_results_panelwise')
    logger.info(f"Writing panelwise results to file '{truncate_filepath(filepath)}'")

    data_keys = [
        ('gamma', 'm^2/s'),
        ('vx', 'm/s'),
        ('vy', 'm/s'),
        ('vz', 'm/s'),
        ('vmag', 'm/s'),
        ('fx', 'N'),
        ('fy', 'N'),
        ('fz', 'N'),
        ('fmag', 'N'),
        ('cp', '1'),
    ]

    # ----- Data and formatting -----
    data = np.stack([vlmdata.panelwise[data_key] for (data_key, _) in data_keys], axis=-1)
    fmt = [FMT_NUM for _ in data_keys]

    # ----- Header -----
    field_size = 12
    header = ''
    for i, (data_key, unit) in enumerate(data_keys):
        header += f'{data_key} [{unit}]'.center(field_size)
        field_size = 14

    # ----- Save -----
    np.savetxt(
        filepath,
        data,
        delimiter=',',
        header=header,
        fmt=fmt,
        comments='# '
    )


def _save_matrix_system(state, vlmdata, settings):
    """
    Save downwash matrix and right-hand side of VLM equation system

    Args:
        :state: (object) data structure for operating conditions
        :vlmdata: (object) data structure for VLM calculation data
        :settings: (object) data structure for execution settings
    """

    filepath = settings.paths('f_results_matrix')
    logger.info(f"Writing system matrix to file '{truncate_filepath(filepath)}'")

    # ----- Data and formatting -----
    matrix = vlmdata.matrix_downwash
    rhs = vlmdata.array_rhs.reshape((vlmdata.matrix_downwash.shape[0], 1))
    data = np.concatenate([matrix, rhs], axis=1)

    # ----- Header -----
    header = f"Downwash matrix: {vlmdata.matrix_downwash.shape}\n"
    header += f"Right-hand side: {vlmdata.array_rhs.shape}"

    # ----- Save -----
    np.savetxt(
        filepath,
        data, delimiter=',',
        header=header,
        fmt=FMT_NUM,
        comments='# '
    )


def save_aeroperformance_map(state, settings):
    """
    Save the aeroperformance map

    Args:
        :state: (object) data structure for operating conditions
        :settings: (object) data structure for execution settings
    """

    filepath = settings.paths('f_results_apm_global')
    logger.info(f"Writing aeroperformance map results to '{truncate_filepath(filepath)}'")

    output = {}
    for dictionary in [state.aero, state.results]:
        for k, v in dictionary.items():
            output[k] = list(v) if v is not None else []

    with open(filepath, "w") as fp:
        dump_pretty_json(output, fp)
