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
Main entry point for plotting

Developed at Airinnova AB, Stockholm, Sweden.
"""

import logging

from . import plottools as pt

from . import downwash as pl_downwash

logger = logging.getLogger(__name__)


def make_all(settings, aircraft, cur_state, vlmdata, lattice):
    """
    Evaluate settings and create plots accordingly

    Args:
        :settings: Settings instance
        :aircraft: Aircraft model
        :vlm_struct: VLM struct
        :cur_state: Current state
        :lattice: Lattice
    """

    def get_plot_settings(plot_name):
        """Return a 'plot_settings' dictionary"""

        plot_settings = settings.settings['plot'][plot_name]
        plot_settings['plot_dir'] = settings.paths('d_plots')
        return plot_settings

    def evaluate_plot(plot_settings):
        """Returns 'True' if 'save' or 'show' is True, otherwise 'False'"""

        return any((plot_settings['save'], plot_settings['show']))

    # ----- Generate plots -----
    plot_settings = get_plot_settings('matrix_downwash')
    if evaluate_plot(plot_settings):
        pl_downwash.view_downwash(vlmdata, plot_settings)

    plot_settings = get_plot_settings('geometry')
    if evaluate_plot(plot_settings):
        plot_geometry_aircraft(aircraft, plot_settings)

    plot_settings = get_plot_settings('lattice')
    if evaluate_plot(plot_settings):
        plot_lattice_aircraft(aircraft, lattice, plot_settings)

    plot_settings = get_plot_settings('results')
    if evaluate_plot(plot_settings):
        plot_results_aircraft(aircraft, lattice, cur_state, vlmdata, plot_settings)


def plot_geometry_aircraft(aircraft, plot_settings):
    """
    Generate 2D and 3D views of full aircraft geometry

    Args:
        :aircraft: (object) data structure for aircraft model
        :plot_settings: Plot settings
    """

    logger.info("Generating geometry plot...")
    with pt.plot2d3d(aircraft, 'geometry', plot_settings) as (figure_2d, axes_2d, figure_3d, axes_3d):
        pt.add_CG(axes_2d, axes_3d, aircraft)
        pt.add_wings(axes_2d, axes_3d, aircraft)
        pt.add_controls(axes_2d, axes_3d, aircraft)


def plot_lattice_aircraft(aircraft, lattice, plot_settings):
    """
    Generate 2D and 3D views of the lattice

    Args:
        :aircraft: (object) data structure for aircraft model
        :plot_settings: Plot settings
    """

    logger.info("Generating lattice plot...")
    with pt.plot2d3d(aircraft, 'lattice', plot_settings) as (figure_2d, axes_2d, figure_3d, axes_3d):
        pt.add_CG(axes_2d, axes_3d, aircraft)
        pt.add_lattice(axes_2d, axes_3d, lattice)

        # Note: controls are plotted in the undeformed state
        if not aircraft.has_deformed_wings:
            pt.add_controls(axes_2d, axes_3d, aircraft)

        if 'deformation' in plot_settings['opt']:
            pt.add_deformation_field_points(axes_2d, axes_3d, aircraft)


def plot_results_aircraft(aircraft, lattice, state, vlmdata, plot_settings):
    """
    Generate results plot

    Args:
        :aircraft: (object) data structure for aircraft model
        :plot_settings: Plot settings
    """

    logger.info("Generating results plot...")
    for key in plot_settings['opt']:
        with pt.plot2d3d(aircraft, f'results_{key}', plot_settings) as (_, axes_2d, figure_3d, axes_3d):
            pt.add_freestream_vector(axes_2d, axes_3d, state)
            pt.add_results(axes_2d, axes_3d, figure_3d, vlmdata, lattice, aircraft, key)
