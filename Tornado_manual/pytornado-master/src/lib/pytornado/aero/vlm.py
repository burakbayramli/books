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
Functions for the discretisation of the aircraft geometry into panels.

Developed for AIRINNOVA AB, Stockholm, Sweden.
"""

import logging
from math import ceil

import numpy as np
import scipy.linalg.lapack as lapack
from commonlibs.math.vectors import axis_rot_matrix
from commonlibs.math.interpolation import lin_interpol

from pytornado.objects.vlm_struct import VLMLattice
import pytornado.aero.c_vlm as c_vlm
import pytornado.objects.objecttools as ot
from pytornado.objects.vlm_struct import BookKeepingEntry
from pytornado.objects.aircraft import get_abs_segment_point_coords

logger = logging.getLogger(__name__)

MIN_AUTOPANELS = 1


def set_autopanels(aircraft, settings):
    """
    Automatically set chord- and spanwise discretisation settings

    Args:
        :aircraft: (object) data structure for aircraft geometry
        :autopanels_c: (int) number of chordwise panels on the main wing
        :autopanels_s: (int) number of spanwise panels on the main wing
    """

    autopanels_c = settings.settings.get('vlm_autopanels_c', MIN_AUTOPANELS)
    autopanels_s = settings.settings.get('vlm_autopanels_s', MIN_AUTOPANELS)

    for this_segment, _ in ot.all_segments(aircraft):
        segment = this_segment[2]

        if segment.panels['num_c'] is None:
            segment.panels['num_c'] = autopanels_c

        if segment.panels['num_s'] is None:
            wing_span = segment.parent_wing.span
            segment_span = segment.geometry['span']
            segment.panels['num_s'] = ceil((segment_span/wing_span)*autopanels_s)

    for this_control, this_wing in ot.all_controls(aircraft):
        control = this_control[2]

        if control.panels['num_c'] is None:
            control.panels['num_c'] = autopanels_c


def pre_panelling(aircraft):
    """
    Create subdivisions and subareas for all aircraft wings

    Note:
        * This routine divides the wing into subdivisions and subareas
          in order to generate a suitable mesh for wing with control surfaces.
        * In a first step "mandatory" subdivisions are made: The wing is divided
          into a minimum amount of subareas according to the leading and trailing
          edge control surfaces.
        * In a second step further spanwise subdivisions are added.

    Args:
        :aircraft: (obj) aircraft object
    """

    # TODO:
    # - Potential problem:
    #   * The algorithm is based on "correct ordering" of segments:
    #     Segments must be ordered from root to tip (check if this always given!?)

    # ===== PART ONE (MANDATORY SUBDIVISIONS) =====

    # For each control we must add suitable subdivisions
    for this_control, this_wing in ot.all_controls(aircraft):
        control = this_control[2]
        wing = this_wing[2]

        # Segment names on which control surface edges are located
        segment_inner_name = control.segment_uid['inner']
        segment_outer_name = control.segment_uid['outer']

        # Control surface geometry
        eta_inner = control.rel_vertices['eta_inner']
        eta_outer = control.rel_vertices['eta_outer']
        xsi_inner = control.rel_vertices['xsi_inner']
        xsi_outer = control.rel_vertices['xsi_outer']

        # Hinge axis
        xsi_h1 = control.rel_hinge_vertices['xsi_inner']
        xsi_h2 = control.rel_hinge_vertices['xsi_outer']

        # ----- CASE (A) -----
        # The left and right edge of the control are located on SAME segment
        if segment_inner_name == segment_outer_name:
            wing.segments[segment_inner_name].add_subdivision_for_control(
                    eta_inner, eta_outer, control,
                    xsi_inner, xsi_outer, xsi_h1, xsi_h2)

        # ----- CASE (B) -----
        # - The control surface spans over one or more segment borders
        # - Now we will make a list of segments over which the control spans
        # - We will interpolate the position of the control and the hinge
        #   axis at segment borders
        else:
            # Create a list of segments which contain the control
            # list_of_segments[0]: segment_uid
            # list_of_segments[1]: eta_inner
            # list_of_segments[2]: eta_outer
            # list_of_segments[3]: xsi_inner
            # list_of_segments[4]: xsi_outer
            # list_of_segments[5]: xsi_h1
            # list_of_segments[6]: xsi_h2
            list_of_segments = []

            # Flags to indicate that the inner or outer control positions have been set
            inner_set = False
            outer_set = False

            # To start with we use averaged values for xsi (geometry and hinge axis)
            xsi_avg = (xsi_inner + xsi_outer)/2
            xsi_h_avg = (xsi_h1 + xsi_h2)/2

            for segment_uid in wing.segments.keys():
                if segment_uid == segment_inner_name:
                    inner_set = True
                    # Note: eta_outer = 1
                    list_of_segments.append([segment_uid, eta_inner, 1, xsi_inner, xsi_avg, xsi_h1, xsi_h_avg])
                    continue

                elif inner_set and not outer_set:
                    # Note: eta_inner = 0
                    # Note: eta_outer = 1
                    list_of_segments.append([segment_uid, 0, 1, xsi_avg, xsi_avg, xsi_h_avg, xsi_h_avg])

                    # If we are on the last segment we must update some incorrectly set values
                    if segment_uid == segment_outer_name:
                        outer_set = True
                        list_of_segments[-1][2] = eta_outer
                        list_of_segments[-1][4] = xsi_outer
                        list_of_segments[-1][6] = xsi_h2
                        break

            # Potentially, we must readjust the control surface geometry/hinge axis at borders
            if (xsi_inner != xsi_outer) or (xsi_h1 != xsi_h2):
                # Let's first compute the "length" of the control surface
                control_len = [0, ]

                for row in list_of_segments:
                    segment_uid, eta_i, eta_o, xsi_i, xsi_o, xsi_hi, xsi_ho = row

                    segment = wing.segments[segment_uid]
                    segment_vertices = segment.vertices

                    a = get_abs_segment_point_coords(segment_vertices, eta_i, xsi_i)
                    b = get_abs_segment_point_coords(segment_vertices, eta_o, xsi_o)
                    ab = b - a

                    # l: total length of control
                    l = control_len[-1]
                    control_len.append(l + np.sqrt(np.dot(ab, ab)))

                l = control_len[-1]

                # Now, we update the xsi values using linear interpolation
                for i, row in enumerate(list_of_segments):
                    segment_uid, eta_i, eta_o, xsi_i, xsi_o, xsi_hi, xsi_ho = row

                    # Update the xsi values
                    l_i = control_len[i]
                    l_o = control_len[i+1]

                    xsi_i = lin_interpol((xsi_inner, xsi_outer), (0, l), l_i)
                    xsi_o = lin_interpol((xsi_inner, xsi_outer), (0, l), l_o)
                    xsi_hi = lin_interpol((xsi_h1, xsi_h2), (0, l), l_i)
                    xsi_ho = lin_interpol((xsi_h1, xsi_h2), (0, l), l_o)

                    list_of_segments[i] = [segment_uid, eta_i, eta_o, xsi_i, xsi_o, xsi_hi, xsi_ho]

            # Finally, we create the subdivisions using our list
            for row in list_of_segments:
                segment_uid, eta_i, eta_o, xsi_i, xsi_o, xsi_h1, xsi_h2 = row

                wing.segments[segment_uid].add_subdivision_for_control(
                        eta_i, eta_o, control, xsi_i, xsi_o, xsi_h1, xsi_h2)

    # ===== PART TWO (ADDITIONAL SPANWISE SUBDIVISIONS) =====

    # - Adding additional spanwise subdivisions is done here in Python rather
    #   than in the C code as it is more convenient to keep track of which
    #   parts (subareas) of the discretised surface have which functions
    for this_segment, _ in ot.all_segments(aircraft):
        segment = this_segment[2]

        for eta in np.linspace(0, 1, segment.panels['num_s']+1):
            if (eta == 0) or (eta == 1):
                continue
            segment.add_subdivision(eta, eta, ignore_inval_eta=True)


def gen_lattice(aircraft, state, settings, make_new_subareas=True):
    """
    Generate aircraft lattice

    Perform count of number of wings, segments, controls, panels and strips.
    Pre-allocate memory for lattice data, which is directly operated on in C.

    The function `py2c_lattice` is called which generates the VLM lattice.
    The following lattice data (for all panel) is generated:

        * :lattice.p: panel corner points
        * :lattice.v: panel vortex filament endpoints
        * :lattice.c: panel collocation point
        * :lattice.n: panel normal vector
        * :lattice.a: panel surface area

    When `py2c_lattice` is called it takes the following input arguments:

        * :lattice: pre-allocated memory for the struct described above
        * :array_segments: segment corner points (N*4*3)
        * :array_airfoils: file names for airfoils at inner and outer segment (N*2)
        * :array_symmetry: segment symmetry information (N)
        * :array_panels: number of chordwise and spanwise panels for each segment (N*2)

    Display lattice metrics in console and log file.

    Args:
        :aircraft: (object) data structure for aircraft geometry
        :state: (object) data structure for flight state
        :settings: (object) data structure for execution settings
        :make_new_subareas: Flag

    Returns:
        :lattice: (object) data structure for VLM lattice
    """

    if make_new_subareas:
        pre_panelling(aircraft)

    # Start the panel bookkeping with a clean slate
    lattice = VLMLattice()
    lattice.clean_bookkeeping()

    logger.info("Getting lattice information ... ")

    num_subareas = 0
    num_r = 0  # total number of panel strips
    num_p = 0  # total number of panels

    # PANEL COUNT AND BOOK KEEPING
    for this_subarea, _, this_segment, this_wing in ot.all_subareas(aircraft):
        wing = this_wing[2]
        segment = this_segment[2]
        subarea = this_subarea[2]

        num_subareas += 1

        pan_idx1 = num_p

        # A subarea only has chordwise subdivisions
        num_chordwise_panels = subarea.parent_control.panels['num_c'] if \
                               subarea.parent_control is not None else \
                               segment.panels['num_c']

        # If a subdivisions has a flap and/or slat we can reduce the number of
        # chordwise subdivision
        if subarea.type == 'segment':
            num_chordwise_panels = ceil(subarea.rel_length*num_chordwise_panels)

        # NOTE: now num_r and num_p are the same (improve !?)
        num_r += num_chordwise_panels
        num_p += num_chordwise_panels

        lattice.update_bookkeeping(
                BookKeepingEntry(subarea, range(pan_idx1, num_p), num_chordwise_panels, mirror=False))

        if wing.symmetry:
            num_subareas += 1

            pan_idx1 = num_p
            num_r += num_chordwise_panels
            num_p += num_chordwise_panels

            lattice.update_bookkeeping(
                    BookKeepingEntry(subarea, range(pan_idx1, num_p), num_chordwise_panels, mirror=True))

    # Make sure integers are stored as integers
    num_wings = int(ot.count_all_wings(aircraft))
    num_controls = int(ot.count_all_controls(aircraft))
    num_subareas = int(num_subareas)
    num_r = int(num_r)
    num_p = int(num_p)

    lattice.info['num_wings'] = num_wings
    lattice.info['num_segments'] = num_subareas
    lattice.info['num_controls'] = num_controls
    lattice.info['num_strips'] = num_r
    lattice.info['num_panels'] = num_p

    logger.info("Pre-allocating lattice memory...")

    array_subareas = np.zeros((num_subareas, 4, 3), dtype=float, order='C')
    array_symmetry = np.zeros((num_subareas), dtype=int, order='C')
    array_panels = np.ones((num_subareas, 2), dtype=int, order='C')

    i = 0
    for entry in lattice.panel_bookkeeping:
        subarea = entry.subarea
        pan_idx = entry.pan_idx
        mirror = entry.mirror

        vertices = subarea.abs_vertices(mirror)

        array_subareas[i, 0, :] = vertices['a']
        array_subareas[i, 1, :] = vertices['b']
        array_subareas[i, 2, :] = vertices['c']
        array_subareas[i, 3, :] = vertices['d']

        # TODO: array_panels can be simplified (now only vector)
        # array_panels[i, 0] = 1
        array_panels[i, 1] = entry.num_chordwise_panels
        i += 1

    # Override symmetry flags (0, do not mirror anything that is passed in)
    array_symmetry = np.zeros((num_p), dtype=int, order='C')

    lattice.p = np.zeros((num_p, 4, 3), dtype=float, order='C')
    lattice.v = np.zeros((num_p, 4, 3), dtype=float, order='C')
    lattice.c = np.zeros((num_p, 3), dtype=float, order='C')
    lattice.bound_leg_midpoints = np.zeros((num_p, 3), dtype=float, order='C')
    lattice.n = np.zeros((num_p, 3), dtype=float, order='C')
    lattice.a = np.zeros((num_p), dtype=float, order='C')
    lattice.epsilon = settings.settings['_epsilon']

    logger.info("Generating lattice...")
    c_vlm.py2c_lattice(lattice, state, array_subareas, array_symmetry, array_panels)

    # ----- Compute the length of the bound leg -----
    logger.info(f"--> Number of panels: {lattice.info['num_panels']}")
    logger.info(f"--> Min panel area = {lattice.info['area_min']:.3e}")
    logger.info(f"--> Max panel area = {lattice.info['area_max']:.3e}")
    logger.info(f"--> Avg panel area = {lattice.info['area_avg']:.3e}")
    logger.info(f"--> Min panel aspect ratio = {lattice.info['aspect_min']:.3e}")
    logger.info(f"--> Max panel aspect ratio = {lattice.info['aspect_max']:.3e}")
    logger.info(f"--> Avg panel aspect ratio = {lattice.info['aspect_avg']:.3e}")

    # ========== ROTATE NORMALS ==========
    if settings.settings['_do_normal_rotations']:
        for entry in lattice.panel_bookkeeping:
            subarea = entry.subarea
            pan_idx = entry.pan_idx
            mirror = entry.mirror

            # CONTROL SURFACE DEFLECTIONS
            if subarea.parent_control is not None:
                hinge_axis = subarea.abs_hinge_axis(mirror)

                if mirror:
                    deflection = subarea.parent_control.deflection_mirror
                else:
                    deflection = subarea.parent_control.deflection

                # If deflection is 0, do not attempt rotation
                if deflection:
                    deflection = np.deg2rad(deflection)
                    R = axis_rot_matrix(hinge_axis, deflection)

                    for i in pan_idx:
                        lattice.n[i, :] = R @ lattice.n[i, :]

            # CAMBER LINE
            eta_a = subarea.parent_subdivision.rel_vertices['eta_a']
            eta_b = subarea.parent_subdivision.rel_vertices['eta_b']
            eta_m = (eta_a + eta_b)/2
            airfoil = subarea.parent_segment.segment_airfoil.at_eta(eta_m)

            num_panels = len([i for i in pan_idx])
            collocation_xsi = subarea.get_xsi_for_collocation_points(num_panels)

            for pan_of_subarea, i in enumerate(pan_idx):
                rot_axis = subarea.abs_camber_line_rot_axis(mirror)
                xsi = collocation_xsi[pan_of_subarea]
                angle = np.deg2rad(airfoil.camber_line_angle(xsi))

                # If deflection is 0, do not attempt rotation
                if angle:
                    R = axis_rot_matrix(rot_axis, angle)
                    lattice.n[i, :] = R @ lattice.n[i, :]
    return lattice


def calc_downwash(lattice, vlmdata):
    """
    Generate downwash factors for aircraft lattice.

    Pre-allocate memory for the (num_p x num_p) downwash factor matrix.
    The downwash calculations are performed in C, directly into this matrix.

    Display matrix condition number in console and log file.

    Args:
        :lattice: (object) data structure for VLM lattice
        :vlmdata: (object) data structure for VLM input and output
    """

    logger.info("Pre-allocating downwash matrix in memory...")
    num_p = lattice.info['num_panels']
    vlmdata.matrix_downwash = np.zeros((num_p, num_p), dtype=float, order='C')

    logger.info("Computing downwash factors...")
    c_vlm.py2c_downwash(lattice, vlmdata.matrix_downwash)
    logger.info(f"--> Condition number = {np.linalg.cond(vlmdata.matrix_downwash):.3e}")


def calc_boundary(lattice, state, vlmdata):
    """
    Generate boundary conditions (RHS term) for VLM.

    Pre-allocate memory for the (num_p x 1) right-hand-side array.
    The right-hand side terms are computed in C, directly into this memory.

    Args:
        :lattice: (object) data structure for VLM lattice
        :state: (object) data structure for flight state
        :vlmdata: (object) data structure for VLM input and output
    """

    logger.info("Pre-allocating rhs array in memory...")
    num_p = lattice.info['num_panels']
    vlmdata.array_rhs = np.zeros((num_p), dtype=float, order='C')

    logger.info("Computing right-hand side term...")
    c_vlm.py2c_boundary(lattice, state, vlmdata.array_rhs)
    vlmdata.array_rhs = np.array(vlmdata.array_rhs)


def solver(vlmdata):
    """
    Solve linear system for vortex strengths

    Args:
        :vlmdata: (object) data structure for VLM input and output
    """

    logger.info("Solving linear system...")
    vlmdata.matrix_lu, vlmdata.array_pivots, vlmdata.panelwise['gamma'], _ \
        = lapack.dgesv(vlmdata.matrix_downwash, vlmdata.array_rhs)

########
########
########
    # Needed???
    # vlmdata.panelwise['gamma'] = np.array(vlmdata.panelwise['gamma'], dtype=float, order='C')
########
########
########


def calc_results(lattice, state, vlmdata):
    """
    Calculate inwash at collocation points

    Args:
        :lattice: (object) data structure for VLM lattice
        :state: (object) data structure for flight state
        :vlmdata: (object) data structure for VLM input and output
    """

    logger.info("Pre-allocating vortex-lattice method results...")
    num_p = lattice.info['num_panels']

    # Allocate memory for panelwise results
    for key in ['vx', 'vy', 'vz', 'vmag',  'fx', 'fy', 'fz', 'fmag', 'cp']:
        vlmdata.panelwise[key] = np.zeros((num_p), dtype=float, order='C')

    logger.info("Computing results...")
    c_vlm.py2c_results(lattice, state, vlmdata)

    logger.info(f"--> Fx = {vlmdata.forces['x']:10.3e}")
    logger.info(f"--> Fy = {vlmdata.forces['y']:10.3e}")
    logger.info(f"--> Fz = {vlmdata.forces['z']:10.3e}")
    logger.info(f"--> FD = {vlmdata.forces['D']:10.3e}")
    logger.info(f"--> FC = {vlmdata.forces['C']:10.3e}")
    logger.info(f"--> FL = {vlmdata.forces['L']:10.3e}")
    logger.info(f"--> Mx = {vlmdata.forces['l']:10.3e}")
    logger.info(f"--> My = {vlmdata.forces['m']:10.3e}")
    logger.info(f"--> Mz = {vlmdata.forces['n']:10.3e}")

    logger.info(f"--> Cx = {vlmdata.coeffs['x']:7.4f}")
    logger.info(f"--> Cy = {vlmdata.coeffs['y']:7.4f}")
    logger.info(f"--> Cz = {vlmdata.coeffs['z']:7.4f}")
    logger.info(f"--> CD = {vlmdata.coeffs['D']:7.4f}")
    logger.info(f"--> CC = {vlmdata.coeffs['C']:7.4f}")
    logger.info(f"--> CL = {vlmdata.coeffs['L']:7.4f}")
    logger.info(f"--> Cl = {vlmdata.coeffs['l']:7.4f}")
    logger.info(f"--> Cm = {vlmdata.coeffs['m']:7.4f}")
    logger.info(f"--> Cn = {vlmdata.coeffs['n']:7.4f}")
