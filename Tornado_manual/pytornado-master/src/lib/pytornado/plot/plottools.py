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
Plot tools and common plot operations

Developed at Airinnova AB, Stockholm, Sweden.
"""

from contextlib import contextmanager
from datetime import datetime
import logging
import os

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from mpl_toolkits.mplot3d import Axes3D
from commonlibs.logger import truncate_filepath
from commonlibs.math.vectors import unit_vector

import pytornado.objects.objecttools as ot

logger = logging.getLogger(__name__)

NUM_POINTS = 100


class _PlotSettings:
    STANDARD_DPI = 200
    STANDARD_FORMAT = 'png'

    # Sizes
    LINEWIDTH_a = 0.5
    LINEWIDTH_b = 1
    LINEWIDTH_c = 2

    POINTSIZE_a = 40

    # Scaling
    SCALE_FACTOR = 1.05


PS = _PlotSettings


class _Colors:
    # ----- Base colors -----
    BLACK = 'black'
    GREY = 'grey'
    GREEN = 'green'
    RED = 'red'
    BLUE = 'blue'
    MAROON = 'maroon'

    # ----- Objects -----
    MESH = BLACK
    MESH_MIRROR = GREY
    CONTROL_SLAT = GREEN
    CONTROL_FLAP = RED
    CONTROL_HINGE = BLUE
    DEFORMATION = MAROON

    # ----- Colormap -----
    COLORMAP = cm.get_cmap('Spectral')


C = _Colors


def _get_limits(points, lims, symmetry=0):
    """
    Determine external limits of domain occupied by set of points

    Args:
        :points: (numpy) Points of current plot object
        :lims: (numpy) Current bounding box
        :symmetry: (int) Symmetry setting
    """

    # Flatten multi-dimensional array of coordinates to (m x n x ...) x 3
    if points.ndim > 2 and points.shape[-1]:
        points = points.reshape(-1, 3)

    lims_min = lims[0]
    lims_max = lims[1]

    # Find limits
    points_min = points.min(axis=0)
    points_max = points.max(axis=0)

    indices_min = points_min < lims_min
    indices_max = points_max > lims_max

    lims_min[indices_min] = points_min[indices_min]
    lims_max[indices_max] = points_max[indices_max]

    # Account for symmetry
    if symmetry == 1:
        if -points_max[2] < lims_min[2]:
            lims_min[2] = -points_max[2]

        if -points_min[2] > lims_max[2]:
            lims_max[2] = -points_min[2]

    if symmetry == 2:
        if -points_max[1] < lims_min[1]:
            lims_min[1] = -points_max[1]

        if -points_min[1] > lims_max[1]:
            lims_max[1] = -points_min[1]

    if symmetry == 3:
        if -points_max[0] < lims_min[0]:
            lims_min[0] = -points_max[0]

        if -points_min[0] > lims_max[0]:
            lims_max[0] = -points_min[0]


def _scale_fig(axes, lims, directions='xyz'):
    """
    Scale axes to ensure unit aspect ratio between plot axes

    Args:
        :axes: (object) matplotlib.axis.Axis object
        :lims: (numpy) Bounding box
        :directions: (string) Combination of x, y, z for axis directions
            * Example: 'zx' -> x-axis = aircraft z-coord, y-axis = aircraft x-coord, no z-axis (2D view)
    """

    # Find largest (half-)range
    lims_min = lims[0]
    lims_max = lims[1]
    range_max = 0.5*(lims_max - lims_min).max(axis=0)

    # Find midpoint of geometry
    x_mid = 0.5*(lims_max[0] + lims_min[0])
    y_mid = 0.5*(lims_max[1] + lims_min[1])
    z_mid = 0.5*(lims_max[2] + lims_min[2])

    domain = {
        'x': (x_mid - PS.SCALE_FACTOR*range_max, x_mid + PS.SCALE_FACTOR*range_max),
        'y': (y_mid - PS.SCALE_FACTOR*range_max, y_mid + PS.SCALE_FACTOR*range_max),
        'z': (z_mid - PS.SCALE_FACTOR*range_max, z_mid + PS.SCALE_FACTOR*range_max),
    }

    axes.set_xlim(domain[directions[0]])
    axes.set_ylim(domain[directions[1]])

    # 3D plot
    if len(directions) == 3:
        axes.set_zlim(domain[directions[2]])


def interpolate_quad(a, b, c, d, size):
    """
    Bi-linear parametrisation of arbitrarily twisted quadrilateral

    Args:
        :a, b, c, d: (numpy) Coordinates of corner vertices
        :nr, ns: (int) Number of chord- and span-wise points

    Returns:
        :x, y, z: (numpy) Coordinates of NC x NS interpolated points
    """

    fr = 0.5*np.sqrt(np.sum((b - a)**2.0)) + 0.5*np.sqrt(np.sum((c - d)**2.0))
    fs = 0.5*np.sqrt(np.sum((d - a)**2.0)) + 0.5*np.sqrt(np.sum((c - b)**2.0))

    nr = int(2 + np.ceil(NUM_POINTS*fr/size))
    ns = int(2 + np.ceil(NUM_POINTS*fs/size))

    r, s = np.meshgrid(np.linspace(0.0, 1.0, num=nr), np.linspace(0.0, 1.0, num=ns))

    alpha1 = a[0]
    alpha2 = b[0] - a[0]
    alpha3 = d[0] - a[0]
    alpha4 = a[0] - b[0] + c[0] - d[0]

    beta1 = a[1]
    beta2 = b[1] - a[1]
    beta3 = d[1] - a[1]
    beta4 = a[1] - b[1] + c[1] - d[1]

    gama1 = a[2]
    gama2 = b[2] - a[2]
    gama3 = d[2] - a[2]
    gama4 = a[2] - b[2] + c[2] - d[2]

    x = alpha1 + alpha2*r + alpha3*s + alpha4*r*s
    y = beta1 + beta2*r + beta3*s + beta4*r*s
    z = gama1 + gama2*r + gama3*s + gama4*r*s
    return x, y, z


@contextmanager
def plot2d3d(aircraft, plot_name, plot_settings):
    """
    Context manager for 2D and 3D plots

    Args:
        :aircraft: Aircraft model
        :plot_name: Name if file is to be saved
        :plot_settings: Dictionary with plot settings

    Yields:
        * A tuple with (figure_2d, axes_2d, figure_3d, axes_3d)

    Note:
        * 'plot_name' is used to generate a file name if plot are to be saved.
          A suffix '2D' or '3D' is added to 'plot_name'
    """

    # ----- 3D plot -----
    figure_3d, axes_3d = _init_plot3d(title=aircraft.uid)
    _add_info_plot3d(axes_3d, aircraft)

    # ----- 2D plot -----
    figure_2d, axes_2d = _init_plot2d(title=aircraft.uid)

    _scale_plots(axes_2d, axes_3d, aircraft)

    try:
        yield (figure_2d, axes_2d, figure_3d, axes_3d)
    # except:
    #     plt.close('all')
    finally:
        show_and_save(plot_settings, (figure_3d, plot_name + '3D'), (figure_2d, plot_name + '2D'))
        plt.close('all')


def _init_plot3d(title=''):
    """
    Initialise an axes object for 3D plots

    Args:
        :title: (str) Plot title

    Returns:
        :figure_3d: Figure object (matplotlib)
        :axes_3d: 3D axes object (matplotlib)
    """

    figure_3d = plt.figure(figsize=(12, 12), edgecolor=C.BLACK)
    axes_3d = figure_3d.gca(projection='3d')

    # --> Matplotlib version 3.1.1 raises NotImplementedError
    # --> See: https://github.com/matplotlib/matplotlib/issues/1077
    #
    # axes_3d.set_aspect('equal')

    # Add labels
    axes_3d.set_xlabel('X [m]')
    axes_3d.set_ylabel('Y [m]')
    axes_3d.set_zlabel('Z [m]')

    axes_3d.set_title(title)

    figure_3d.subplots_adjust(
        left=0.00,
        bottom=0.01,
        right=0.90,
        top=0.96,
        wspace=0.39,
        hspace=0.45
    )

    return figure_3d, axes_3d


def _init_plot2d(title=''):
    """
    Initialise an axes object for 2D plots

    Args:
        :title: (str) Plot title

    Returns:
        :figure_2d: Figure object (matplotlib)
        :axes_2d: 2D axes object (matplotlib)
    """

    figure_2d = plt.figure(figsize=(20, 7), edgecolor=C.BLACK)
    axes_yz = figure_2d.add_subplot(131)
    axes_xz = figure_2d.add_subplot(132)
    axes_xy = figure_2d.add_subplot(133)
    axes_yz.set_aspect('equal')
    axes_xz.set_aspect('equal')
    axes_xy.set_aspect('equal')

    # Add labels
    axes_yz.set_xlabel('Y [m]')
    axes_yz.set_ylabel('Z [m]')

    axes_xz.set_xlabel('X [m]')
    axes_xz.set_ylabel('Z [m]')

    axes_xy.set_xlabel('X [m]')
    axes_xy.set_ylabel('Y [m]')

    # Add titles
    axes_yz.set_title("Y-Z plane")
    axes_xz.set_title("X-Z plane")
    axes_xy.set_title("X-Y plane")

    figure_2d.suptitle(title)
    return figure_2d, (axes_yz, axes_xz, axes_xy)


def _scale_plots(axes_2d, axes_3d, aircraft):
    """
    Correct the axes scaling

    Args:
        :axes_2d: 2D axes object (matplotlib)
        :axes_3d: 3D axes object (matplotlib)
        :aircraft: Aircraft model
    """

    axes_yz, axes_xz, axes_xy = axes_2d

    # Iterate through segment vertices to determine required plot dimension
    lims = np.zeros((2, 3))
    for (_, _, segment), (_, _, wing) in ot.all_segments(aircraft):
        points = np.array([segment.vertices['a'],
                           segment.vertices['b'],
                           segment.vertices['c'],
                           segment.vertices['d'],
                           segment.vertices['a']])
        _get_limits(points, lims, symmetry=wing.symmetry)

    # Adjust scaling for all axes objects
    _scale_fig(axes_3d, lims)
    _scale_fig(axes_yz, lims, directions='yz')
    _scale_fig(axes_xz, lims, directions='xz')
    _scale_fig(axes_xy, lims, directions='xy')


def _add_CG_plot3d(axes_3d, aircraft):
    """
    Add a marker indicating the centre of gravity

    Args:
        :axes_3d: 3D axes object (matplotlib)
        :aircraft: (object) data structure for aircraft model
    """

    X, Y, Z = aircraft.refs['gcenter']
    axes_3d.scatter(X, Y, Z, color=C.BLACK, marker='x', s=PS.POINTSIZE_a, linewidth=PS.LINEWIDTH_c)


def _add_CG_plot2d(axes_2d, aircraft):
    """
    Add a marker indicating the centre of gravity

    Args:
        :axes_2d: 2D axes object (matplotlib)
        :aircraft: (object) data structure for aircraft model
    """

    X, Y, Z = aircraft.refs['gcenter']
    axes_yz, axes_xz, axes_xy = axes_2d

    axes_yz.scatter(Y, Z, color=C.BLACK, marker='x', s=PS.POINTSIZE_a, linewidth=PS.LINEWIDTH_c)
    axes_xz.scatter(X, Z, color=C.BLACK, marker='x', s=PS.POINTSIZE_a, linewidth=PS.LINEWIDTH_c)
    axes_xy.scatter(X, Y, color=C.BLACK, marker='x', s=PS.POINTSIZE_a, linewidth=PS.LINEWIDTH_c)


def add_CG(axes_2d, axes_3d, aircraft):
    """
    Add a marker indicating the centre of gravity

    Args:
        :axes_2d: 2D axes object (matplotlib)
        :axes_3d: 3D axes object (matplotlib)
        :aircraft: (object) data structure for aircraft model
    """

    _add_CG_plot3d(axes_3d, aircraft)
    _add_CG_plot2d(axes_2d, aircraft)


def add_wings(axes_2d, axes_3d, aircraft):
    """
    Add wings (segment vertices) to axes objects

    Args:
        :axes_2d: 2D axes object (matplotlib)
        :axes_3d: 3D axes object (matplotlib)
        :aircraft: (object) data structure for aircraft model
    """

    axes_yz, axes_xz, axes_xy = axes_2d
    for (_, _, segment), (_, _, wing) in ot.all_segments(aircraft):
        points = np.array([
            segment.vertices['a'],
            segment.vertices['b'],
            segment.vertices['c'],
            segment.vertices['d'],
            segment.vertices['a'],
        ])
        _plot_XYZ_points(axes_2d, axes_3d, points, wing.symmetry, linewidth=PS.LINEWIDTH_b, color=C.MESH_MIRROR)

        # TODO
        # Add 'segment.center' --> 'wing.center'
        # center = np.mean(M, axis=0)
        # text = axes_3d.text(center[0], centre[1], centre[2], wing_uid, backgroundcolor='w', size='medium')
        # text.set_bbox(dict(color='w', alpha=0.4))
        # ============


def add_controls(axes_2d, axes_3d, aircraft):
    """
    Add control surfaces to axes objects

    Args:
        :axes_2d: 2D axes object (matplotlib)
        :axes_3d: 3D axes object (matplotlib)
        :aircraft: (object) data structure for aircraft model
    """

    axes_yz, axes_xz, axes_xy = axes_2d
    for (_, _, control), (_, _, wing) in ot.all_controls(aircraft):
        # ----- Add outer control geometry -----
        color = C.CONTROL_FLAP if control.device_type == 'flap' else C.CONTROL_SLAT
        vertex_order = 'dabc' if control.device_type == 'flap' else 'bcda'
        points = np.array([control.abs_vertices[vertex_name] for vertex_name in vertex_order])
        _plot_XYZ_points(axes_2d, axes_3d, points, wing.symmetry, linewidth=PS.LINEWIDTH_c, color=color)

        # ----- Add hinges -----
        points = np.array([
            control.abs_hinge_vertices['p_inner'],
            control.abs_hinge_vertices['p_outer'],
        ])
        _plot_XYZ_points(axes_2d, axes_3d, points, wing.symmetry, linewidth=PS.LINEWIDTH_c, color=C.CONTROL_HINGE)


def _add_info_plot3d(axes_3d, aircraft):
    """
    Add info box to 3D plot

    Args:
        :axes_3d: Axes object (matplotlib)
        :aircraft: Aircraft model
    """

    axes_3d.annotate(
        f"Wings = {ot.count_all_wings(aircraft):2d}\n"
        + f"Segments = {ot.count_all_segments(aircraft):2d}\n"
        + f"Controls = {ot.count_all_controls(aircraft):2d}\n\n"
        + f"Aircraft size = {aircraft.size:5.2f}",
        xy=(0, 0),
        xytext=(1, 0),
        textcoords='axes fraction',
        va='bottom',
        ha='right'
    )


def show_and_save(plot_settings, *figures):
    """
    Save and/or show plots

    Args:
        :plot_settings: Plot settings
        :figures: Tuples with (figure_object, 'name_of_plot')
    """

    plt.tight_layout()

    if plot_settings['save']:
        for figure, fig_name in figures:
            now = datetime.now().strftime("%F_%H-%M-%S-%f")
            fname = os.path.join(
                plot_settings['plot_dir'],
                f"{fig_name}_{now}.{PS.STANDARD_FORMAT}"
            )
            logger.info(f"Saving plot as file: '{truncate_filepath(fname)}'")
            figure.savefig(fname, dpi=PS.STANDARD_DPI, format=PS.STANDARD_FORMAT)

    if plot_settings['show']:
        plt.show()


def add_lattice(axes_2d, axes_3d, lattice):
    """
    Add the VLM mesh

    Args:
        :axes_2d: 2D axes object (matplotlib)
        :axes_3d: 3D axes object (matplotlib)
        :lattice: Lattice object
    """

    axes_yz, axes_xz, axes_xy = axes_2d
    for pp, pc, pv, pn in zip(lattice.p, lattice.c, lattice.v, lattice.n):
        # PANELS
        points_p = np.array([pp[0], pp[1], pp[2], pp[3], pp[0]])
        _plot_XYZ_points(axes_2d, axes_3d, points_p, symmetry=None, linewidth=PS.LINEWIDTH_a, color=C.MESH)

        # # ==========
        # opt_settings = ['normals', 'horseshoes', 'horseshoe_midpoints']
        # # ==========

        # # PLOT THE NORMALS
        # if 'normals' in opt_settings:
        #     points_c2n = np.array([pc, pc+pn])

        #     X = points_c2n[:, 0]
        #     Y = points_c2n[:, 1]
        #     Z = points_c2n[:, 2]

        #     axes_3d.plot(X, Y, Z, color='blue', linewidth=0.5)
        #     axes_yz.plot(Y, Z, color='blue', linewidth=0.5)
        #     axes_xz.plot(X, Z, color='blue', linewidth=0.5)
        #     axes_xy.plot(X, Y, color='blue', linewidth=0.5)

        # # PLOT VORTEX POINTS
        # if 'horseshoes' in opt_settings:
        #     points_v = np.array([pv[0],
        #                          pv[1],
        #                          pv[2],
        #                          pv[3],
        #                          pv[0]])

        #     X = points_v[:, 0]
        #     Y = points_v[:, 1]
        #     Z = points_v[:, 2]

        #     axes_3d.plot(X, Y, Z, color='green', linewidth=0.25)
        #     axes_yz.plot(Y, Z, color='green', linewidth=0.25)
        #     axes_xz.plot(X, Z, color='green', linewidth=0.25)
        #     axes_xy.plot(X, Y, color='green', linewidth=0.25)

    # if 'horseshoe_midpoints' in opt_settings:
        # for bound_leg_midpoint in lattice.bound_leg_midpoints:
        #     axes_3d.scatter(*bound_leg_midpoint, marker='.', s=10, color='red')


# ===========================================================================
# ===========================================================================
# ===========================================================================

        # # PLOT SUBDIVISIONS
        # if 'subdivisions' in opt_settings:
        #     for this_subarea, _, _, _ in all_subareas(aircraft):
        #         subarea_type = this_subarea[1]
        #         subarea = this_subarea[2]

        #         if subarea_type == 'segment':
        #             color = 'blue'
        #         elif subarea_type == 'slat':
        #             color = 'green'
        #         elif subarea_type == 'flap':
        #             color = 'red'

        #         mirror_list = [False]
        #         if subarea.symmetry:
        #             mirror_list.append(True)

        #         for mirror in mirror_list:
        #             vertices = subarea.abs_vertices(mirror)
        #             points = np.array([vertices['a'],
        #                                vertices['b'],
        #                                vertices['c'],
        #                                vertices['d'],
        #                                vertices['a']])

        #             X = points[:, 0]
        #             Y = points[:, 1]
        #             Z = points[:, 2]

        #             axes_xyz.plot(X, Y, Z, color=color, marker='.', linewidth=1.0, markersize=4.0)
        #             axes_yz.plot(Y, Z, color=color, linewidth=1.0)
        #             axes_xz.plot(X, Z, color=color, linewidth=1.0)
        #             axes_xy.plot(X, Y, color=color, linewidth=1.0)

        #             # Camber line local axis
        #             if 'camberline_rot_axis' in opt_settings:
        #                 camber_axis_vertices = subarea.abs_camber_line_rot_axis_vertices(mirror)
        #                 inner = camber_axis_vertices['p_inner']
        #                 outer = camber_axis_vertices['p_outer']
        #                 subarea_vertices = subarea.abs_vertices(mirror)
        #                 axes_xyz.quiver(*((subarea_vertices['a'] + subarea_vertices['b'])/2),
        #                                 *(outer - inner), color='orange', linewidth=1)

        #             if subarea_type in ['flap', 'slat']:
        #                 hinge_vertices = subarea.abs_hinge_vertices(mirror)
        #                 hinge_axis = subarea.abs_hinge_axis(mirror)

        #                 x, y, z = hinge_vertices['p_inner']
        #                 u, v, w = hinge_axis
        #                 axes_xyz.quiver(x, y, z, u, v, w, color='black', linewidth=1)

        #                 points = np.array([hinge_vertices['p_inner'], hinge_vertices['p_outer']])
        #                 X = points[:, 0]
        #                 Y = points[:, 1]
        #                 Z = points[:, 2]

        #                 axes_yz.plot(Y, Z, '--', color='black', linewidth=1.0)
        #                 axes_xz.plot(X, Z, '--', color='black', linewidth=1.0)
        #                 axes_xy.plot(X, Y, '--', color='black', linewidth=1.0)

# ===========================================================================
# ===========================================================================
# ===========================================================================


def add_deformation_field_points(axes_2d, axes_3d, aircraft):
    """
    Plot the deformation field points

    Args:
        :axes_2d: 2D axes object (matplotlib)
        :axes_3d: 3D axes object (matplotlib)
        :aircraft: (obj) aircraft
    """

    axes_yz, axes_xz, axes_xy = axes_2d
    for wing in aircraft.wings.values():
        if wing.is_deformed:
            for def_field in (wing.def_field, wing.def_field_mirror):
                if def_field is not None:
                    points = def_field[:, 0:3]
                    _plot_XYZ_points(
                        axes_2d, axes_3d, points, symmetry=0,
                        linewidth=PS.LINEWIDTH_c, color=C.MAROON,
                        marker='o'
                    )


def add_freestream_vector(axes_2d, axes_3d, state, tip_pos=np.array([0, 0, 0]), vector_len=3):
    """
    Add a free stream vector

    Args:
        :axes_2d: 2D axes object (matplotlib)
        :axes_3d: 3D axes object (matplotlib)
        :state: State model
    """

    free_stream_vel = vector_len*unit_vector(state.free_stream_velocity_vector)
    orig = tip_pos - free_stream_vel
    axes_3d.quiver(*orig, *free_stream_vel, color=C.BLACK, linewidth=PS.LINEWIDTH_c)
    axes_3d.text(*orig, f"{state.aero['airspeed']:.1f} m/s")


def add_results(axes_2d, axes_3d, figure_3d, vlmdata, lattice, aircraft, key):
    """
    Add results

    Args:
        :axes_2d: 2D axes object (matplotlib)
        :axes_3d: 3D axes object (matplotlib)
        :figure_3d: Figure object (matplotlib)
        :vlmdata: (object) data structure for VLM analysis data
        :lattice: Lattice object
        :aircraft: Aircraft model
        :key: Property to plot
    """

    axes_yz, axes_xz, axes_xy = axes_2d

    # Normalise to range [0, 1]
    data = vlmdata.panelwise[key]
    val_range = max(data) - min(data)
    if val_range != 0:
        values = (data - min(data))/val_range
    else:
        values = np.zeros(data.shape)

    for pp, val in zip(lattice.p, values):
        color = C.COLORMAP(val)
        points_p = np.array([pp[0], pp[1], pp[2], pp[3], pp[0]])

        XS, YS, ZS = interpolate_quad(points_p[0], points_p[1], points_p[2], points_p[3], size=aircraft.size)
        axes_3d.plot_surface(XS, YS, ZS, color=color, linewidth=PS.LINEWIDTH_a, shade=False, cstride=1, rstride=1)
        axes_yz.fill(YS, ZS, color=color, facecolor=color, fill=True)
        axes_xz.fill(XS, ZS, color=color, facecolor=color, fill=True)
        axes_xy.fill(XS, YS, color=color, facecolor=color, fill=True)

    cbar = cm.ScalarMappable(cmap=C.COLORMAP)
    cbar.set_array(vlmdata.panelwise[key])
    cb = figure_3d.colorbar(cbar)
    cb.set_label(key)


def _plot_XYZ_points(axes_2d, axes_3d, points, symmetry, *, linewidth, color,
                     color_mirror=None, marker=None):
    """
    Plot a group of XYZ coordinates

    Args:
        :axes_2d: 2D axes object (matplotlib)
        :axes_3d: 3D axes object (matplotlib)
        :XYZ: Tuple with coordinates (X, Y, Z)
        :symmetry: Symmetry flag
        :linewidth: Linewidth
        :color: Color of the primary side
        :color_mirror: Color of the mirrored side (None if same as 'color')
    """

    X = points[:, 0]
    Y = points[:, 1]
    Z = points[:, 2]
    axes_yz, axes_xz, axes_xy = axes_2d
    color_mirror = color if color_mirror is None else color_mirror

    axes_3d.plot(X, Y, Z, color=color, linewidth=linewidth, marker=marker)
    axes_yz.plot(Y, Z, color=color, linewidth=linewidth, marker=marker)
    axes_xz.plot(X, Z, color=color, linewidth=linewidth, marker=marker)
    axes_xy.plot(X, Y, color=color, linewidth=linewidth, marker=marker)

    # X-Y symmetry
    if symmetry == 1:
        axes_3d.plot(X, Y, -Z, color=color, linewidth=linewidth)
        axes_yz.plot(Y, -Z, color=color, linewidth=linewidth)
        axes_xz.plot(X, -Z, color=color, linewidth=linewidth)

    # X-Z symmetry
    elif symmetry == 2:
        axes_3d.plot(X, -Y, Z, color=color, linewidth=linewidth)
        axes_yz.plot(-Y, Z, color=color, linewidth=linewidth)
        axes_xy.plot(X, -Y, color=color, linewidth=linewidth)

    # Y-Z symmetry
    elif symmetry == 3:
        axes_3d.plot(-X, Y, Z, color=color, linewidth=linewidth)
        axes_xz.plot(-X, Z, color=color, linewidth=linewidth)
        axes_xy.plot(-X, Y, color=color, linewidth=linewidth)
