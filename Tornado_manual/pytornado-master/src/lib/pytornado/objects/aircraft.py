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
Aircraft data model. The aircraft hierarchy is defined as follows::


    |         AIRCRAFT
    |            |
    |           WING
    |            |
    |           / \\
    |          /   \\
    |         /     \\
    |     SEGMENT   CONTROL
    |        |
    |      STRIP
    |        |
    |    SUBDIVISION


Developed for Airinnova AB, Stockholm, Sweden.
"""


from collections import OrderedDict
from copy import copy
from math import sqrt, sin, cos, tan, asin, atan2, degrees, radians
import logging
import os

import numpy as np
from commonlibs.math.vectors import get_plane_line_intersect
from airfoils import Airfoil, MorphAirfoil
from airfoils.fileio import import_airfoil_data
from aeroframe.interpol.translate import get_deformed_point
from schemadict import schemadict


# Global unit vectors
Y_AXIS = np.array([0, 1, 0])
Z_AXIS = np.array([0, 0, 1])

TOL = 1.0e-02

MIN_XSI_LIMIT = 0.01
MIN_ETA_LIMIT = 0.01

# Common schema entries
SCHEMA_FLOAT_01 = {'type': float, '>=': 0.0, '<=': 1.0}
SCHEMA_ARRAY_XYZ = {'type': list, 'min_len': 3, 'max_len': 3, 'item_types': (int, float)}
SCHEMA_FLOAT_POS = {'type': float, '>': 0}
SCHEMA_STRING = {'type': str}

logger = logging.getLogger(__name__)


class ComponentDefinitionError(Exception):
    """Raised if when a component is ill-defined"""

    pass


class Aircraft:

    # Schema for reference values
    REF_SCHEMA = schemadict({
        '$required_keys': ['gcenter', 'rcenter', 'area', 'chord', 'span'],
        'gcenter': SCHEMA_ARRAY_XYZ,
        'rcenter': SCHEMA_ARRAY_XYZ,
        'area': SCHEMA_FLOAT_POS,
        'chord': SCHEMA_FLOAT_POS,
        'span': SCHEMA_FLOAT_POS,
        'comment': SCHEMA_STRING,
    })

    def __init__(self):
        """
        Aircraft model

        The aircraft object is broken down into child objects:

            * The 'Aircraft' has 'Wing's
            * A 'Wing' has 'WingSegment's
            * A 'Wing' has 'WingControl's
            * A 'WingSegment' has 'SegmentStrip's
            * A 'SegmentStrip' has 'StripSubdivision's

        The following objects have unique identifiers (uid):

            * Aircraft
            * Wing
            * WingSegment
            * WingControl

        Deformation API:

            * A 'Wing' can have a deformation field attributed to it
            * All deformations will be interpolated from this deformation field

        Attributes:
            :uid: (string) Unique identifier
            :refs: (dict) Reference values for coefficients
            :wing: (dict) Wing objects
            :size: (float) Aircraft geometry size measure
            :area: (float) Aircraft surface area
            :state: (bool) Component definition state
        """

        self._uid = 'aircraft'

        self.refs = {}
        self.refs['area'] = None
        self.refs['span'] = None
        self.refs['chord'] = None
        self.refs['gcenter'] = None
        self.refs['rcenter'] = None
        self.refs['comment'] = None

        # Wing child objects
        self.wings = OrderedDict()

        self._state = False

    @property
    def uid(self):
        return self._uid

    @uid.setter
    def uid(self, uid):
        if not isinstance(uid, str):
            raise TypeError("'uid' must be a string")
        self._uid = uid

    @property
    def area(self):
        """Aircraft area as sum of wing areas"""

        area = 0
        for wing_uid, wing in self.wings.items():
            area += wing.area
        return area

    @property
    def size(self):
        """
        Approximate characteristic size of aircraft (diagonal of a bounding box)
        """

        bbox = np.zeros((2, 3))
        for wing_uid, wing in self.wings.items():
            for segment_uid, segment in wing.segments.items():
                # Segment vertices
                points = np.array(list(segment.vertices.values()))
                points_min = points.min(axis=0)
                points_max = points.max(axis=0)

                indices_min = points_min < bbox[0, :]
                indices_max = points_max > bbox[1, :]

                # Update bounding box
                bbox[0, indices_min] = points_min[indices_min]
                bbox[1, indices_max] = points_max[indices_max]

                # Segment centre weighted by area
                center = 0.25*sum(points)

                # Account for symmetry
                if wing.symmetry == 1:
                    if -points_max[2] < bbox[0, 2]:
                        bbox[0, 2] = -points_max[2]
                    if -points_min[2] > bbox[1, 2]:
                        bbox[1, 2] = -points_min[2]
                    center[2] = 0.0

                if wing.symmetry == 2:
                    if -points_max[1] < bbox[0, 1]:
                        bbox[0, 1] = -points_max[1]
                    if -points_min[1] > bbox[1, 1]:
                        bbox[1, 1] = -points_min[1]
                    center[1] = 0.0

                if wing.symmetry == 3:
                    if -points_max[0] < bbox[0, 0]:
                        bbox[0, 0] = -points_max[0]
                    if -points_min[0] > bbox[1, 0]:
                        bbox[1, 0] = -points_min[0]
                    center[0] = 0.0

        size = np.linalg.norm(bbox[1, :] - bbox[0, :])
        return size

    @property
    def state(self):
        return self._state

    @property
    def has_deformed_wings(self):
        """True if any wing are deformed"""

        for wing in self.wings.values():
            if wing.is_deformed:
                return True
        return False

    def add_wing(self, wing_uid):
        """
        Add a new wing object

        Args:
            :wing_uid: (string) Unique identifier for the wing

        Returns:
            :wing: (obj) Created wing object
        """

        if not wing_uid:
            raise ComponentDefinitionError("Empty UID string!")
        elif wing_uid in self.wings:
            raise ValueError(f"Wing '{wing_uid}' is already defined!")

        self.wings.update({wing_uid: Wing(wing_uid)})
        return self.wings[wing_uid]

    def generate(self):
        """Generate the aircraft model"""

        logger.debug(f"Generating aircraft '{self.uid}'...")

        # ----- Reference values -----
        logger.info("Checking reference values...")
        self.refs['area'] = float(self.refs['area'])
        self.refs['chord'] = float(self.refs['chord'])
        self.refs['span'] = float(self.refs['span'])
        self.refs['gcenter'] = list(self.refs['gcenter'])
        self.refs['rcenter'] = list(self.refs['rcenter'])

        self.REF_SCHEMA.validate(self.refs)

        self.refs['gcenter'] = np.array(self.refs['gcenter'], dtype=float, order='C')
        self.refs['rcenter'] = np.array(self.refs['rcenter'], dtype=float, order='C')

        # ----- Child objects -----
        for wing_uid, wing in self.wings.items():
            logger.debug(f"Generating wing '{wing_uid}'...")
            # Check data and generate segment, control, area, span
            wing.generate()

        self._state = True


class Wing:

    def __init__(self, wing_uid):
        """
        Wing model (child of Aircraft)

        Args:
            :wing_uid: (str) Unique identifier

        Attr:
            :span: (float) Total span of WingSegment components
            :area: (float) total area of WingSegment components
            :segment: (dict) Wing child objects: WingSegment
            :control: (dict) Wing child objects: WingControl
            :symmetry: (int) Wing symmetry (0: none; 1: yz; 2: xz; 3: xy)
            :state: (bool) Wing definition state
        """

        self.uid = wing_uid
        self._symmetry = 0

        # Child objects (segment and control surfaces)
        self.segments = OrderedDict()
        self.controls = OrderedDict()

        # Deformation fields
        self.def_field = None
        self.def_field_mirror = None

        self._state = False

    @property
    def uid(self):
        return self._uid

    @uid.setter
    def uid(self, uid):
        if not isinstance(uid, str):
            raise TypeError("'uid' must be a string")
        self._uid = uid

    @property
    def state(self):
        return self._state

    @property
    def symmetry(self):
        return self._symmetry

    @symmetry.setter
    def symmetry(self, symmetry):
        if symmetry not in (0, 1, 2, 3):
            raise ValueError("'symmetry' must be 0, 1, 2 or 3.")
        self._symmetry = symmetry

    @property
    def is_deformed(self):
        if self.def_field is not None or self.def_field_mirror is not None:
            return True
        else:
            False

    @property
    def area(self):
        """Return the wing area"""

        area = 0.0
        for segment in self.segments.values():
            area += segment.area
        return area

    @property
    def span(self):
        """Return the wing span"""

        span = 0.0
        for segment in self.segments.values():
            span += abs(segment.geometry['span'])
        return span

    def add_segment(self, segment_uid):
        """
        Add a new segment

        Args:
            :segment_uid: (string) Segment identifier

        Returns:
            :segment: (obj) New segment object
        """

        if not segment_uid:
            raise ComponentDefinitionError("Empty name string!")
        elif segment_uid in self.segments.keys():
            raise ValueError(f"segment '{segment_uid}' already exists!")

        self.segments.update({segment_uid: WingSegment(self, segment_uid)})
        return self.segments[segment_uid]

    def add_control(self, control_uid):
        """
        Add a new control surface

        Args:
            :control_uid: (string) Control identifier

        Returns:
            :control: (obj) New segment object
        """

        if not control_uid:
            raise ComponentDefinitionError("Empty name string!")
        elif control_uid in self.controls.keys():
            raise ValueError(f"Control '{control_uid}' already exists!")

        self.controls.update({control_uid: WingControl(self, control_uid)})
        return self.controls[control_uid]

    def generate(self):
        """
        Generate a wing and components
        """

        # TODO: Improve continuity check, currently only collinearity

        segments = list(self.segments.values())

        # Generate wing segments
        for segment in segments:
            segment.generate()

        # ----- Calculate segment positions -----
        pos = 0.0
        for segment in segments:
            segment.position['inner'] = pos/self.span
            pos += segment.geometry['span']
            segment.position['outer'] = pos/self.span

        # ----- Check wing continuity -----
        # TODO: iterate pairwise!!!
        wing_continuous = True
        for i in range(len(segments) - 1):
            # Segment edge directions
            segm_1 = segments[i]
            segm_2 = segments[i + 1]

            a1d1 = abs(segm_1.vertices['d'] - segm_1.vertices['a'])
            a1d1 = a1d1/np.linalg.norm(a1d1)

            b1c1 = abs(segm_1.vertices['c'] - segm_1.vertices['b'])
            b1c1 = b1c1/np.linalg.norm(b1c1)

            a2d2 = abs(segm_2.vertices['d'] - segm_2.vertices['a'])
            a2d2 = a2d2/np.linalg.norm(a2d2)

            b2c2 = abs(segm_2.vertices['c'] - segm_2.vertices['b'])
            b2c2 = b2c2/np.linalg.norm(b2c2)

            # Cross-segment vertex-to-vertex directions
            a1c2 = abs(segm_2.vertices['c'] - segm_1.vertices['a'])
            a1c2 = a1c2/np.linalg.norm(a1c2) if np.linalg.norm(a1c2) else np.zeros(3)

            d1c2 = abs(segm_2.vertices['c'] - segm_1.vertices['d'])
            d1c2 = d1c2/np.linalg.norm(d1c2) if np.linalg.norm(d1c2) else np.zeros(3)

            b1d2 = abs(segm_2.vertices['d'] - segm_1.vertices['b'])
            b1d2 = b1d2/np.linalg.norm(b1d2) if np.linalg.norm(b1d2) else np.zeros(3)

            c1d2 = abs(segm_2.vertices['d'] - segm_1.vertices['c'])
            c1d2 = c1d2/np.linalg.norm(c1d2) if np.linalg.norm(c1d2) else np.zeros(3)

            b1c2 = abs(segm_2.vertices['c'] - segm_1.vertices['b'])
            b1c2 = b1c2/np.linalg.norm(b1c2) if np.linalg.norm(b1c2) else np.zeros(3)

            c1c2 = abs(segm_2.vertices['c'] - segm_1.vertices['c'])
            c1c2 = c1c2/np.linalg.norm(c1c2) if np.linalg.norm(c1c2) else np.zeros(3)

            a1d2 = abs(segm_2.vertices['d'] - segm_1.vertices['a'])
            a1d2 = a1d2/np.linalg.norm(a1d2) if np.linalg.norm(a1d2) else np.zeros(3)

            d1d2 = abs(segm_2.vertices['d'] - segm_1.vertices['d'])
            d1d2 = d1d2/np.linalg.norm(d1d2) if np.linalg.norm(d1d2) else np.zeros(3)

            segment_continuous = False
            # B1C1 and A2D2 have same orientation
            if np.linalg.norm(a2d2 - b1c1) < TOL:
                # B1C1 and A2D2 collinear
                if np.linalg.norm(a2d2 - b1d2) < TOL or np.linalg.norm(a2d2 - c1d2) < TOL:
                    # if not np.linalg.norm(b1d2) + np.linalg.norm(c1d2) > np.linalg.norm(b1c1):
                    #     #   B1C1 overlaps A2D2
                    segment_continuous = True
                    logger.debug(f"Edge {i}-{i+1} is continuous (root-to-tip)")

            # A1D1 and B2C2 have same orientation
            elif np.linalg.norm(b2c2 - a1d1) < TOL:
                # A1D1 and B2C2 collinear
                if np.linalg.norm(b2c2 - a1c2) < TOL or np.linalg.norm(b2c2 - d1c2) < TOL:
                    # if not np.linalg.norm(a1c2) + np.linalg.norm(d1c2) > np.linalg.norm(a1d1):
                    #     #   A1D1 overlaps B2C2
                    segment_continuous = True
                    logger.debug(f"Edge {i}-{i+1} is continuous (tip-to-root)")

            # B1C1 and B2C2 have same orientation
            elif np.linalg.norm(b2c2 - b1c1) < TOL:
                # B1C1 and B2C2 collinear
                if np.linalg.norm(b2c2 - b1c2) < TOL or np.linalg.norm(b2c2 - c1c2) < TOL:
                    # if not np.linalg.norm(b1c2) + linalg.norm(c1c2) > np.linalg.norm(b1c1):
                    #     #   B1C1 overlaps B2C2
                    segment_continuous = True
                    logger.debug(f"Edge {i}-{i+1} is continuous (with discontinuous normal!)")

            # A1D1 and A2D2 have same orientation
            elif np.linalg.norm(a2d2 - a1d1) < TOL:
                # A1D1 and A2D2 collinear
                if np.linalg.norm(a2d2 - a1d2) < TOL or np.linalg.norm(a2d2 - d1d2) < TOL:
                    # if np.linalg.norm(a1d2) + np.linalg.norm(d1d2) > np.rm(d1d2) <= max_dist:
                    #     #   A1D1 overlaps A2D2
                    segment_continuous = True
                    logger.debug(f"Edge {i}-{i+1} is continuous (with discontinuous normal!)")

            if not segment_continuous:
                logger.warning(f"Edge {i}-{i+1} is discontinuous")

            wing_continuous &= segment_continuous

        if not wing_continuous:
            logger.warning("Wing is discontinuous!")

        # ----- Wing has been checked -----
        self._state = True

    def check_deformation_continuity(self):
        """
        Check the continuity of the wing deformation

        The deformation at the border of two neighbouring segments must be
        the same. Otherwise, there will be "gaps" in the wing which we will
        refuse to model.
        """

        logger.info("Checking continuity of wing deformation...")

        raise NotImplementedError


class WingSegment:

    # Schema for vertices
    VERTICES_SCHEMA = schemadict({
        # Vertices are not "required" input
        'a': SCHEMA_ARRAY_XYZ,
        'b': SCHEMA_ARRAY_XYZ,
        'c': SCHEMA_ARRAY_XYZ,
        'd': SCHEMA_ARRAY_XYZ,
    })

    # Schema for geometry
    GEOMETRY_SCHEMA = schemadict({
        '$required_keys': [
            'inner_axis',
            'outer_axis',
        ],
        'inner_chord': {'type': float},
        'outer_chord': {'type': float},
        'inner_alpha': {'type': float, '>=': -90, '<=': +90},
        'outer_alpha': {'type': float, '>=': -90, '<=': +90},
        'inner_beta': {'type': float, '>=': -90, '<=': +90},
        'outer_beta': {'type': float, '>=': -90, '<=': +90},
        'inner_axis': SCHEMA_FLOAT_01,
        'outer_axis': SCHEMA_FLOAT_01,
        'span': {'type': float},
        'sweep': {'type': float, '>=': -90, '<=': +90},
        'dihedral': {'type': float, '>=': -180, '<=': +180},
    })

    AIRFOIL_SCHEMA = schemadict({
        '$required_keys': ['inner', 'outer'],
        'inner': {'type': str},
        'outer': {'type': str},
    })

    PANELS_SCHEMA = schemadict({
        # Entries can be None (i.e. no required keys)
        'num_c': {'type': int, '>': 0},
        'num_s': {'type': int, '>': 0},
    })

    def __init__(self, wing, uid):
        """
        Wing segment (child of Wing class)

        A wing segment represents a quadrilateral segment of lifting surface

        Attributes:
            :parent_wing: (obj) Parent wing object
            :area: (float) Surface area
            :position: (dict) Span-wise position in WING
            :vertices: (dict) Corner point coordinates
            :geometry: (dict) Geometric properties
            :airfoil: (dict) Wing section coordinates
            :panels: (dict) Discretization properties
            :state: (bool) Definition state
        """

        self.parent_wing = wing
        self.uid = uid

        # Calculated position in wing (span) and lattice (index)
        self.position = {}
        self.position['inner'] = None
        self.position['outer'] = None
        self.position['panel'] = None

        # Provided or calculated segment area
        self.vertices = {}
        self.vertices['a'] = None
        self.vertices['b'] = None
        self.vertices['c'] = None
        self.vertices['d'] = None

        # Provided or calculated segment properties
        self.geometry = {}
        self.geometry['inner_chord'] = None
        self.geometry['inner_alpha'] = None
        self.geometry['inner_beta'] = None
        self.geometry['inner_axis'] = None
        self.geometry['outer_chord'] = None
        self.geometry['outer_alpha'] = None
        self.geometry['outer_beta'] = None
        self.geometry['outer_axis'] = None
        self.geometry['span'] = None
        self.geometry['sweep'] = None
        self.geometry['dihedral'] = None

        # Provided airfoil names
        self.airfoils = {}
        self.airfoils['inner'] = None
        self.airfoils['outer'] = None

        # Derived
        self.segment_airfoil = None

        # Provided discretisation settings
        self.panels = {}
        self.panels['num_c'] = None
        self.panels['num_s'] = None

        # Subdivisions (by default there is always one "division")
        self.subdivision = OrderedDict()
        subdivision_rel_vertices = {}
        subdivision_rel_vertices['eta_a'] = 0.0
        subdivision_rel_vertices['eta_b'] = 1.0
        subdivision_rel_vertices['eta_c'] = 1.0
        subdivision_rel_vertices['eta_d'] = 0.0
        self.subdivision.update({0: SegmentStrip(self, subdivision_rel_vertices)})

        # state of component definition (see CHECK)
        self.state = False

    @property
    def uid(self):
        return self._uid

    @uid.setter
    def uid(self, uid):
        if not isinstance(uid, str):
            raise TypeError("'uid' must be a string")
        self._uid = uid

    @property
    def area(self):
        return self.geometry['span']*0.5*(
            self.geometry['inner_chord'] + self.geometry['outer_chord']
        )

    @property
    def normal_vector(self):
        """Segment normal vector (not normalised)"""

        a = self.vertices['a']
        b = self.vertices['b']
        d = self.vertices['d']

        ab = b - a
        ad = d - a

        return np.cross(ab, ad)

    @property
    def main_direction(self):
        """
        Direction of segment

        * Perpendicular to line AD (not normalised)
        * Points roughly from AD to BC
        """

        a = self.vertices['a']
        d = self.vertices['d']

        ad = d - a

        return np.cross(ad, self.normal_vector)

    @property
    def symmetry(self):
        """Symmetry inherited from parent wing"""

        return self.parent_wing.symmetry

    def _import_airfoils(self):
        """
        Import airfoil data, and create a corresponding segment airfoil object

        The airfoils can be imported from:

            * x, y coordinates stored in a file
            * NACA4 definition
        """

        # Stores inner and outer airfoil object
        airfoil_dict = {}

        for position in ['inner', 'outer']:
            airfoil_definition = str(self.airfoils[position])

            import_from_file = False
            import_from_NACA = False

            if os.path.isfile(airfoil_definition):
                logger.info(f"Importing airfoil from file: '{airfoil_definition}'")
                import_from_file = True
            elif airfoil_definition.upper().startswith('NACA'):
                airfoil_definition = airfoil_definition.upper()
                logger.info(f"Importing airfoil from NACA definition ({airfoil_definition})")
                import_from_NACA = True
            else:
                raise ValueError(
                    f"""
                    Airfoil input data neither a valid file nor NACA definition:
                    ==> '{airfoil_definition}'
                    """
                )

            if import_from_file:
                upper, lower = import_airfoil_data(airfoil_definition)
                airfoil_dict[position] = Airfoil(upper, lower)
            elif import_from_NACA:
                naca_digits = str(airfoil_definition).upper().strip('NACA')
                airfoil_dict[position] = Airfoil.NACA4(naca_digits)

        # Create a segment airfoil as a "morphable" airfoil
        self.segment_airfoil = MorphAirfoil(airfoil_dict['inner'], airfoil_dict['outer'])

    def add_subdivision(self, eta_a, eta_d, ignore_inval_eta=False):
        """
        Add a subdivision for the current segment

        Note:
            * A new division will not be created if:
                * a division already exists at same position
                * a division line is very close to an existing division
            * In both cases the cases a handle to the already existing division
              will be returned.

        Args:
            :eta_a: (float) upper relative position of the division line
            :eta_d: (float) upper relative position of the division line
            :ignore_inval_eta: (bool) optional parameter to ignore invalid eta values

        Returns:
            :subvivision: (obj) newly created subdivision object
        """

        # Do not even try to proceed if eta is not within reasonable range
        if eta_a <= 0 or eta_a >= 1:
            logger.warning(f"Cannot add subdivision at eta = {eta_a:.1f}")

            if ignore_inval_eta:
                return None
            else:
                raise ValueError("eta must be in range (0, 1)")

        idx_new = len(self.subdivision)
        idx_prev, prev_subdivision, prev_is_on_border = self._get_subdiv_at_eta(eta_a)

        if prev_is_on_border:
            logger.debug(f"Refusing to create new division at existing division (eta_a = {eta_a:.3f})")
            return self.subdivision[idx_prev]

        if (eta_a - prev_subdivision.rel_vertices['eta_a']) < MIN_ETA_LIMIT:
            logger.debug("Refusing to create a subdivision close to existing division "
                         "(Delta eta_a = {:.3f})".format(eta_a - prev_subdivision.rel_vertices['eta_a']))
            return self.subdivision[idx_prev]

        # Relative vertices of new subdivision
        subdivision_rel_vertices = {}
        subdivision_rel_vertices['eta_a'] = eta_a
        subdivision_rel_vertices['eta_b'] = prev_subdivision.rel_vertices['eta_b']
        subdivision_rel_vertices['eta_c'] = prev_subdivision.rel_vertices['eta_c']
        subdivision_rel_vertices['eta_d'] = eta_d
        self.subdivision.update({idx_new: SegmentStrip(self, subdivision_rel_vertices)})

        eta_a_prev = prev_subdivision.rel_vertices['eta_a']
        eta_b_prev = prev_subdivision.rel_vertices['eta_b']

        # Update relative vertices of old subdivision
        prev_subdivision.rel_vertices['eta_b'] = eta_a
        prev_subdivision.rel_vertices['eta_c'] = eta_d

        # Inherit flap from previous subdivision
        if 'flap' in self.subdivision[idx_prev].subarea.keys():
            xsi1_prev = prev_subdivision.subarea['flap'].rel_vertices['xsi_a']
            xsi2_prev = prev_subdivision.subarea['flap'].rel_vertices['xsi_b']

            xsi_h1_prev = prev_subdivision.subarea['flap'].rel_hinge_vertices['xsi_h1']
            xsi_h2_prev = prev_subdivision.subarea['flap'].rel_hinge_vertices['xsi_h2']

            xsi_sdl = xsi_interpol(self.vertices, (eta_a_prev, xsi1_prev), (eta_b_prev, xsi2_prev), eta_a)
            xsi_h_sdl = xsi_interpol(self.vertices, (eta_a_prev, xsi_h1_prev), (eta_b_prev, xsi_h2_prev), eta_a)

            prev_sd_par_cntrl = prev_subdivision.subarea['flap'].parent_control
            self.subdivision[idx_new]._add_subarea(prev_sd_par_cntrl, xsi_sdl, xsi2_prev, xsi_h_sdl, xsi_h2_prev)
            prev_subdivision._update_subarea('flap', xsi1_prev, xsi_sdl, xsi_h1_prev, xsi_h_sdl)

        # Inherit slat from previous subdivision
        if 'slat' in self.subdivision[idx_prev].subarea.keys():
            xsi1_prev = prev_subdivision.subarea['slat'].rel_vertices['xsi_d']
            xsi2_prev = prev_subdivision.subarea['slat'].rel_vertices['xsi_c']

            xsi_h1_prev = prev_subdivision.subarea['slat'].rel_hinge_vertices['xsi_h1']
            xsi_h2_prev = prev_subdivision.subarea['slat'].rel_hinge_vertices['xsi_h2']

            xsi_sdl = xsi_interpol(self.vertices, (eta_a_prev, xsi1_prev), (eta_b_prev, xsi2_prev), eta_a)
            xsi_h_sdl = xsi_interpol(self.vertices, (eta_a_prev, xsi_h1_prev), (eta_b_prev, xsi_h2_prev), eta_a)

            prev_sd_par_cntrl = prev_subdivision.subarea['slat'].parent_control
            self.subdivision[idx_new]._add_subarea(prev_sd_par_cntrl, xsi_sdl, xsi2_prev, xsi_h_sdl, xsi_h2_prev)
            prev_subdivision._update_subarea('slat', xsi1_prev, xsi_sdl, xsi_h1_prev, xsi_h_sdl)

        return self.subdivision[idx_new]

    def _get_subdiv_at_eta(self, eta):
        """
        Get the subdivision and subdivision index at a specified eta position.

        Args:
            :eta: (float) eta position

        Returns:
            :idx: (int) subdivision index of subdivision object at eta position
            :subdivision: (obj) subdivision object at eta position
            :is_on_border: (bool) flag which indicates if eta is on edge of existing subdivision

        Note:
            * If 'is_on_border' is True, the subdivision with eta_a == eta is returned
        """

        for idx, subdivision in self.subdivision.items():
            if subdivision.rel_vertices['eta_a'] <= eta < subdivision.rel_vertices['eta_b']:
                if eta == subdivision.rel_vertices['eta_a']:
                    logger.debug(f"There is a subdivision line at requested position (eta = {eta:.3f})")
                    return idx, subdivision, True
                else:
                    return idx, subdivision, False

    def _get_outer_neighbour_subdiv(self, eta_a):
        """
        Get the outer subdivision neighbour for a given eta_a.

        Returns:
            :outer_neighbour: (obj) outer neighbour or None if no neighbour found
        """

        # Account for numerical inaccuracies
        eta_a += MIN_ETA_LIMIT/10

        eta_diff_min = 1
        outer_neighbour = None

        for subdivision in self.subdivision.values():
            eta_diff = eta_a - subdivision.rel_vertices['eta_a']

            if eta_diff_min > eta_diff >= 0:
                eta_diff_min = eta_diff
                outer_neighbour = subdivision

        return outer_neighbour

    def add_subdivision_for_control(self, eta_a, eta_b, parent_control, xsi1, xsi2, xsi_h1=None, xsi_h2=None):
        """
        Add a subdivision for with a control device for a specified range

        Args:
            :eta_a: (float) inner eta position of control surface
            :eta_b: (float) outer eta position of control surface
            :parent_control: (obj) reference to the parent control surface
            :xsi1: (float) inner xsi position of control surface
            :xsi2: (float) outer xsi position of control surface
            :xsi_h1: (float) [optional] inner position of the hinge line (=xsi1 if not specified)
            :xsi_h2: (float) [optional] outer position of the hinge line (=xsi2 if not specified)

        Returns:
            :sd_list: (obj-list) list of subdivision objects
        """

        if xsi_h1 is None:
            xsi_h1 = xsi1
            logger.warning("Hinge position xsi_h1 is not defined (assuming xsi1).")

        if xsi_h2 is None:
            xsi_h2 = xsi2
            logger.warning("Hinge position xsi_h2 is not defined (assuming xsi2).")

        # Add a first subdivision (sd1) beginning at eta_a
        if eta_a == 0:
            sd1 = self.subdivision[0]
        else:
            sd1 = self.add_subdivision(eta_a, eta_a)

        # Where does sd1 end? (i.e. what is the eta_b position?)
        eta_b_sd1 = sd1.rel_vertices['eta_b']

        # Make a list of subdivisions incl. inner and outer positions of the subareas
        sd_list = [[sd1, xsi1, xsi2, xsi_h1, xsi_h2]]

        # CASE 1: Newly created subdivisions ends exactly on desired line
        if eta_b == eta_b_sd1:
            pass

        # CASE 2: There is no more subdivisions before eta_b
        elif eta_b < eta_b_sd1:
            self.add_subdivision(eta_b, eta_b)

        # CASE 3: The newly created subdivision ends before desired line
        elif eta_b > eta_b_sd1:
            outer_neighbour = self._get_outer_neighbour_subdiv(eta_b_sd1)
            eta_b_rn = outer_neighbour.rel_vertices['eta_b']

            # Update inner and outer coordinates for subarea lines
            xsi_div = xsi_interpol(self.vertices, (eta_a, xsi1), (eta_b, xsi2), eta_b_sd1)
            xsi_h_div = xsi_interpol(self.vertices, (eta_a, xsi_h1), (eta_b, xsi_h2), eta_b_sd1)

            # xsi2 for the first subdivision must be updated
            sd_list[0][2] = xsi_div
            sd_list[0][4] = xsi_h_div

            # Assume the control for outer neighbour will span from xsi_div to xsi2
            sd_list.append([outer_neighbour, xsi_div, xsi2, xsi_h_div, xsi_h2])

            # If the outer neighbour cannot accommodate the control we have to continue
            n = 0
            while eta_b > eta_b_rn:
                outer_neighbour = self._get_outer_neighbour_subdiv(eta_b_rn)
                eta_b_rn = outer_neighbour.rel_vertices['eta_b']
                eta_b_ln = sd_list[-1][0].rel_vertices['eta_b']

                xsi_div = xsi_interpol(self.vertices, (eta_a, xsi1), (eta_b, xsi2), eta_b_ln)
                xsi_h_div = xsi_interpol(self.vertices, (eta_a, xsi_h1), (eta_b, xsi_h2), eta_b_ln)

                sd_list[-1][2] = xsi_div
                sd_list[-1][4] = xsi_h_div
                sd_list.append([outer_neighbour, xsi_div, xsi2, xsi_h_div, xsi_h2])

                n += 1
                if n > 1/MIN_ETA_LIMIT:
                    logger.error("Too many loops")
                    raise RuntimeError("Too many loops")

            # If the last outer neighbour is larger than required, make a subdivision
            if eta_b < outer_neighbour.rel_vertices['eta_b']:
                self.add_subdivision(eta_b, eta_b)

        # Add subarea for all subdivisions
        for row in sd_list:
            subdivision, xsi1, xsi2, xsi_h1, xsi_h2 = row
            subdivision._add_subarea(parent_control, xsi1, xsi2, xsi_h1, xsi_h2)

        return sd_list[:][0]

    def get_deformed_segment_point(self, eta, xsi, mirror=False):
        """
        Return a point of the deformed segment based on relative coordinates
        of the undeformed segment.

        Args:
            :eta: (float) eta coordinate
            :xsi: (float) xsi coordinate

        Returns:
            :point: segment point in the deformed mesh
        """

        point = get_abs_segment_point_coords(self.vertices, eta, xsi)

        # ========
        # Temporary workaround
        # TODO: handle mirror
        if mirror:
            point = mirror_point(point, plane=self.symmetry)
            def_field = self.parent_wing.def_field_mirror
        else:
            def_field = self.parent_wing.def_field
        # ========

        if def_field is None:
            return point

        def_point = get_deformed_point(point, def_field)
        return def_point

    def generate(self):
        """
        Compute wingsegment vertices and geometry from provided data

        Procedure is as follows:

            * Check if wing segment properties are correctly defined
            * Check if wing segment definition is complete
            * Generate wing segment vertices, geometry if possible

        Note:

            * Points are computed from geometric properties specified in geometry
            * At least one of A, B, C, D must be defined as reference point

        * Will compute corresponding properties in GEOMETRY given points A, D
        * Will compute corresponding properties in GEOMETRY given points B, C
        * Will compute all properties in GEOMETRY given points A, B, C, D

        No other combination of properties and coordinates is accepted

        Correct ordering of the vertices A, B, C, D is enforced:

            * A = (x_min, y_min, z_a) or A = (x_min, y_a, z_min) if vertical
            * B = (x_min, y_max, z_b) or B = (x_min, y_a, z_max) if vertical
            * C = (x_max, y_max, z_c) or C = (x_max, y_a, z_max) if vertical
            * D = (x_max, y_min, z_d) or D = (x_max, y_a, z_min) if vertical
        """

        # ----- Check input data -----
        self.check_vertices()
        self.check_geometry()
        self.check_airfoils()
        self.check_panels()

        # ----- Check provided geometric properties -----
        # All geometric properties provided
        if all(v for v in self.geometry.values()):
            required = ['a', 'b', 'c', 'd', 'ad', 'bc', 'abcd']
        # All geometric properties provided except for inner edge
        elif all(v for k, v in self.geometry.items() if 'inner_' not in k):
            required = ['ad', 'abcd']
        # All geometric properties provided except for outer edge
        elif all(v for k, v in self.geometry.items() if 'outer_' not in k):
            required = ['bc', 'abcd']
        # Some geometric properties provided, but not enough
        elif any(v for v in self.geometry.values()):
            required = ['abcd']
        # No geometric properties provided
        else:
            required = ['abcd']

        # ----- Check provided vertex coordinates -----
        # String of keys of correctly-defined vertices
        provided = ''.join(sorted(k for k, v in self.vertices.items() if v is not None))
        if not provided:
            raise ComponentDefinitionError("No reference point provided")
        if provided not in ['a', 'b', 'c', 'd', 'ad', 'bc', 'abcd']:
            raise ComponentDefinitionError(f"Unsupported definition")

        logger.info("Reference points provided: {}".format(', '.join(c for c in provided)))
        for point in provided:
            logger.info(f"--> Point {point.upper()} = {self.vertices[point]}")

        # ----- Check component definition -----
        # provided inputs are insufficient to generate segment geometry
        if provided not in required:
            self.state = False
            raise ComponentDefinitionError("geometric properties of segment ill-defined.")

        # ----- Generate edge A, D -----
        if not ('a' in provided and 'd' in provided):
            logger.debug("Computing chord AD...")

            tan_ai = tan(radians(self.geometry['inner_alpha']))
            cos_bi = cos(radians(self.geometry['inner_beta']))
            sin_bi = sin(radians(self.geometry['inner_beta']))

            r_i = self.geometry['inner_chord']/sqrt(1.0 + tan_ai**2.0*cos_bi**2.0)

            # Point A
            a_x = +0.0
            a_y = +0.0
            a_z = +0.0

            # Point D relative to point A
            d_x = +r_i*cos_bi
            d_y = +r_i*sin_bi
            d_z = -r_i*cos_bi*tan_ai
        else:
            logger.debug("Computing properties of chord BC...")

            # Point A
            a_x = +0.0
            a_y = +0.0
            a_z = +0.0

            # Point D relative to point A
            d_x = self.vertices['d'][0] - self.vertices['a'][0]
            d_y = self.vertices['d'][1] - self.vertices['a'][1]
            d_z = self.vertices['d'][2] - self.vertices['a'][2]

            r_i = sqrt(d_x*d_x + d_y*d_y)

            self.geometry['inner_chord'] = sqrt(d_x*d_x + d_y*d_y + d_z*d_z)

            cos_bi = +d_x/r_i
            sin_bi = +d_y/r_i

            # atan2() handles singularity
            self.geometry['inner_beta'] = -degrees(asin(sin_bi))
            self.geometry['inner_alpha'] = -degrees(atan2(d_z, r_i*cos_bi))

            tan_ai = tan(radians(self.geometry['inner_alpha']))

            logger.debug(f"--> Inner_chord = {self.geometry['inner_chord']}")
            logger.debug(f"--> Inner_alpha = {self.geometry['inner_alpha']}")
            logger.debug(f"--> Inner_beta = {self.geometry['inner_beta']}")

        # ----- Generate edge B, C -----
        if not ('a' in provided and 'b' in provided):
            logger.debug("Computing chord BC...")

            tan_ao = tan(radians(self.geometry['outer_alpha']))
            cos_bo = cos(radians(self.geometry['outer_beta']))
            sin_bo = sin(radians(self.geometry['outer_beta']))

            r_o = self.geometry['outer_chord']/sqrt(1.0 + tan_ao*tan_ao*cos_bo*cos_bo)

            # Point B
            b_x = +0.0
            b_y = +0.0
            b_z = +0.0

            # Point C relative to point B
            c_x = +r_o*cos_bo
            c_y = +r_o*sin_bo
            c_z = -r_o*cos_bo*tan_ao
        else:
            logger.debug("Computing chord BC...")

            # Point B
            b_x = +0.0
            b_y = +0.0
            b_z = +0.0

            # Point C relative to point B
            c_x = self.vertices['c'][0] - self.vertices['b'][0]
            c_y = self.vertices['c'][1] - self.vertices['b'][1]
            c_z = self.vertices['c'][2] - self.vertices['b'][2]

            r_o = sqrt(c_x*c_x + c_y*c_y)

            self.geometry['outer_chord'] = sqrt(c_x*c_x + c_y*c_y + c_z*c_z)

            cos_bo = +c_x/r_o
            sin_bo = +c_y/r_o

            self.geometry['outer_beta'] = -degrees(asin(sin_bo))
            self.geometry['outer_alpha'] = -degrees(atan2(c_z, r_o*cos_bo))

            tan_ao = tan(radians(self.geometry['outer_alpha']))

            logger.debug(f"--> Outer_chord = {self.geometry['outer_chord']}")
            logger.debug(f"--> Outer_alpha = {self.geometry['outer_alpha']}")
            logger.debug(f"--> Outer_beta = {self.geometry['outer_beta']}")

        # ----- Generate edges A-B AND D-C -----
        # Set default
        if self.geometry['inner_axis'] is None:
            self.geometry['inner_axis'] = 0.25
        if self.geometry['outer_axis'] is None:
            self.geometry['outer_axis'] = 0.25

        # axi_x = r_i*self.geometry['inner_axis']*cos_bi
        axi_y = r_i*self.geometry['inner_axis']*sin_bi
        axi_z = r_i*self.geometry['inner_axis']*cos_bi*tan_ai
        # Offset of point A wrt axis point (dihedral line) on AD

        # axo_x = r_o*self.geometry['outer_axis']*cos_bo
        axo_y = r_o*self.geometry['outer_axis']*sin_bo
        axo_z = r_o*self.geometry['outer_axis']*cos_bo*tan_ao
        # Offset of point B wrt axis point (dihedral line) on BC

        # Relative y, z-offset between axis on B, C-edge and axis on A, B-edge
        axs_y = axo_y - axi_y
        axs_z = axo_z - axi_z

        logger.debug(f"--> Inner_axis = {self.geometry['inner_axis']}")
        logger.debug(f"--> Outer_axis = {self.geometry['outer_axis']}")

        # ------------------------------------------------------------

        # Compute spanwise properties from A, B, C, D
        if provided == 'abcd':
            logger.debug("Computing span-wise properties...")

            b_x = self.vertices['b'][0] - self.vertices['a'][0]
            b_y = self.vertices['b'][1] - self.vertices['a'][1]
            b_z = self.vertices['b'][2] - self.vertices['a'][2]

            c_x = self.vertices['b'][0] - self.vertices['a'][0]
            c_y = self.vertices['b'][1] - self.vertices['a'][1]
            c_z = self.vertices['b'][2] - self.vertices['a'][2]

            # Span measured at leading edge
            self.geometry['span'] = sqrt(b_y*b_y + b_z*b_z)
            # Sweep applied at leading edge
            self.geometry['sweep'] = degrees(atan2(b_x, self.geometry['span']))
            # Dihedral applied along axis
            self.geometry['dihedral'] = degrees(atan2((b_z + axs_z), (b_y + axs_y)))

            logger.debug(f"--> Dihedral = {self.geometry['dihedral']}")
            logger.debug(f"--> Sweep = {self.geometry['sweep']}")
            logger.debug(f"--> Span = {self.geometry['span']}")
        else:
            # Position edge B-C wrt edge A-D
            logger.debug("Computing span-wise edges AB and CD...")

            cos_d = cos(radians(self.geometry['dihedral']))
            sin_d = sin(radians(self.geometry['dihedral']))
            tan_s = tan(radians(self.geometry['sweep']))

            # Effective geometric dihedral of leading edge
            axr = axs_z*cos_d - axs_y*sin_d
            dihedral_ax = radians(self.geometry['dihedral']) - asin(axr/self.geometry['span'])

            b_x += self.geometry['span']*tan_s
            b_y += self.geometry['span']*cos(dihedral_ax)
            b_z += self.geometry['span']*sin(dihedral_ax)

            c_x += self.geometry['span']*tan_s
            c_y += self.geometry['span']*cos(dihedral_ax)
            c_z += self.geometry['span']*sin(dihedral_ax)

            # First point in user-provided points serves as reference
            point = provided[0]

            # Used to set reference to [0.0, 0.0, 0.0] in relative coord
            adjust = {
                'a': [a_x, a_y, a_z],
                'b': [b_x, b_y, b_z],
                'c': [c_x, c_y, c_z],
                'd': [d_x, d_y, d_z],
            }

            # Translate [0.0, 0.0, 0.0] by absolute position of reference
            a_x += self.vertices[point][0] - adjust[point][0]
            a_y += self.vertices[point][1] - adjust[point][1]
            a_z += self.vertices[point][2] - adjust[point][2]

            b_x += self.vertices[point][0] - adjust[point][0]
            b_y += self.vertices[point][1] - adjust[point][1]
            b_z += self.vertices[point][2] - adjust[point][2]

            c_x += self.vertices[point][0] - adjust[point][0]
            c_y += self.vertices[point][1] - adjust[point][1]
            c_z += self.vertices[point][2] - adjust[point][2]

            d_x += self.vertices[point][0] - adjust[point][0]
            d_y += self.vertices[point][1] - adjust[point][1]
            d_z += self.vertices[point][2] - adjust[point][2]

            # Invert span-wise orientation for negative span
            if self.geometry['span'] < 0:
                a_x, b_x, c_x, d_x = b_x, a_x, d_x, c_x
                a_y, b_y, c_y, d_y = b_y, a_y, d_y, c_y
                a_z, b_z, c_z, d_z = b_z, a_z, d_z, c_z

            # Invert span-wise orientation for x < -90.0 or x > +90.0
            if abs(self.geometry['dihedral']) > 90.0:
                a_x, b_x, c_x, d_x = b_x, a_x, d_x, c_x
                a_y, b_y, c_y, d_y = b_y, a_y, d_y, c_y
                a_z, b_z, c_z, d_z = b_z, a_z, d_z, c_z

            # Invert ib chord-wise orientation for negative ib chord
            if self.geometry['inner_chord'] < 0:
                a_x, b_x, c_x, d_x = d_x, b_x, c_x, a_x
                a_y, b_y, c_y, d_y = d_y, b_y, c_y, a_y
                a_z, b_z, c_z, d_z = d_z, b_z, c_z, a_z

            # Invert ib chord-wise orientation for x < -90.0 or x > +90.0
            if abs(self.geometry['inner_alpha']) > 90.0:
                a_x, b_x, c_x, d_x = d_x, b_x, c_x, a_x
                a_y, b_y, c_y, d_y = d_y, b_y, c_y, a_y
                a_z, b_z, c_z, d_z = d_z, b_z, c_z, a_z

            # Invert ib chord-wise orientation for x < -90.0 or x > +90.0
            if abs(self.geometry['inner_beta']) > 90.0:
                a_x, b_x, c_x, d_x = d_x, b_x, c_x, a_x
                a_y, b_y, c_y, d_y = d_y, b_y, c_y, a_y
                a_z, b_z, c_z, d_z = d_z, b_z, c_z, a_z

            # Invert ob chord-wise orientation for negative ob chord
            if self.geometry['outer_chord'] < 0:
                a_x, b_x, c_x, d_x = a_x, c_x, b_x, d_x
                a_y, b_y, c_y, d_y = a_y, c_y, b_y, d_y
                a_z, b_z, c_z, d_z = a_z, c_z, b_z, d_z

            # Invert ob chord-wise orientation for x < -90.0 or x > +90.0
            if abs(self.geometry['outer_alpha']) > 90.0:
                a_x, b_x, c_x, d_x = a_x, c_x, b_x, d_x
                a_y, b_y, c_y, d_y = a_y, c_y, b_y, d_y
                a_z, b_z, c_z, d_z = a_z, c_z, b_z, d_z

            # Invert ob chord-wise orientation for x < -90.0 or x > +90.0
            if abs(self.geometry['outer_beta']) > 90.0:
                a_x, b_x, c_x, d_x = a_x, c_x, b_x, d_x
                a_y, b_y, c_y, d_y = a_y, c_y, b_y, d_y
                a_z, b_z, c_z, d_z = a_z, c_z, b_z, d_z

            self.vertices['a'] = [a_x, a_y, a_z]
            self.vertices['b'] = [b_x, b_y, b_z]
            self.vertices['c'] = [c_x, c_y, c_z]
            self.vertices['d'] = [d_x, d_y, d_z]

        logger.debug(f"--> Vertex a = {self.vertices['a']}")
        logger.debug(f"--> Vertex b = {self.vertices['b']}")
        logger.debug(f"--> Vertex c = {self.vertices['c']}")
        logger.debug(f"--> Vertex d = {self.vertices['d']}")

        # Generate airfoil object
        self._import_airfoils()
        self.state = True

    def check_vertices(self):
        """Check types and values of properties in vertices dictionary"""

        logger.info("Checking vertex coordinates...")
        self.VERTICES_SCHEMA.validate(self.vertices)

        # Convert to NUMPY array
        for key in ['a', 'b', 'c', 'd']:
            self.vertices[key] = np.array(self.vertices[key], dtype=float, order='C')

    def check_geometry(self):
        """Check type and value of geometry dictionary"""

        logger.info("Checking geometric properties...")

        for axis in ['inner_axis', 'outer_axis']:
            if self.geometry[axis] is None:
                self.geometry[axis] = 0.25

        self.GEOMETRY_SCHEMA.validate(self.geometry)

    def check_airfoils(self):
        """Check type and value of airfoil dictionary"""

        logger.info("Checking airfoil properties...")
        self.AIRFOIL_SCHEMA.validate(self.airfoils)

    def check_panels(self):
        """Check type and value of properties in WINGSEGMENT.PANELS."""

        logger.info("Checking discretisation properties...")
        self.PANELS_SCHEMA.validate(self.panels)


class WingControl:

    DEVICE_TYPES = ('slat', 'flap')

    SEGMENT_UID_SCHEMA = schemadict({
        '$required_keys': ['inner', 'outer'],
        'inner': {'type': str},
        'outer': {'type': str},
    })

    PANELS_SCHEMA = schemadict({
        'num_c': {'type': int, '>': 0},
    })

    REL_VERTICES_SCHEMA = schemadict({
        '$required_keys': ['eta_inner', 'eta_outer', 'xsi_inner', 'xsi_outer'],
        'eta_inner': SCHEMA_FLOAT_01,
        'eta_outer': SCHEMA_FLOAT_01,
        'xsi_inner': SCHEMA_FLOAT_01,
        'xsi_outer': SCHEMA_FLOAT_01,
    })

    REL_HINGE_VERTICES_SCHEMA = schemadict({
        '$required_keys': ['xsi_inner', 'xsi_outer'],
        'xsi_inner': SCHEMA_FLOAT_01,
        'xsi_outer': SCHEMA_FLOAT_01,
    })

    def __init__(self, parent_wing, control_uid):
        """
        Wing control (child of Wing class)

        Attributes:
            :parent_wing: (obj) Parent wing object
            :device: (string) WINGCONTROL device type
            :deflection: (float) WINGCONTROL deflection angle
            :vertices: (dict) WINGCONTROL vertex coordinates
            :geometry: (dict) WINGCONTROL geometric properties
            :state: (bool) WINGCONTROL definition state
        """

        self.parent_wing = parent_wing
        self.uid = control_uid

        self._device_type = None
        self.deflection = None
        self.deflection_mirror = None

        # Name of the section on which the inner and outer part of the control section lies on
        self.segment_uid = {}
        self.segment_uid['inner'] = None
        self.segment_uid['outer'] = None

        self.rel_vertices = {}
        self.rel_vertices['eta_inner'] = None
        self.rel_vertices['eta_outer'] = None
        self.rel_vertices['xsi_inner'] = None
        self.rel_vertices['xsi_outer'] = None

        self.rel_hinge_vertices = {}
        self.rel_hinge_vertices['xsi_inner'] = None
        self.rel_hinge_vertices['xsi_outer'] = None

        self.panels = {}
        self.panels['num_c'] = None

    @property
    def device_type(self):
        return self._device_type

    @device_type.setter
    def device_type(self, device_type):
        if not isinstance(device_type, str):
            raise TypeError("'device_type' must be string")

        if device_type not in self.DEVICE_TYPES:
            raise ValueError(f"'device_type' must be in {self.DEVICE_TYPES}")

        self._device_type = device_type

    @property
    def uid(self):
        return self._uid

    @uid.setter
    def uid(self, uid):
        if not isinstance(uid, str):
            raise TypeError("'uid' must be a string")
        self._uid = uid

    @property
    def symmetry(self):
        """Symmetry inherited from parent wing"""

        return self.parent_wing.symmetry

    @property
    def abs_vertices(self):
        """Get absolute coordinates of the control"""

        inner_seg_name = self.segment_uid['inner']
        outer_seg_name = self.segment_uid['outer']
        inner_seg_vertices = self.parent_wing.segments[inner_seg_name].vertices
        outer_seg_vertices = self.parent_wing.segments[outer_seg_name].vertices

        eta_inner = self.rel_vertices['eta_inner']
        xsi_inner = self.rel_vertices['xsi_inner']
        eta_outer = self.rel_vertices['eta_outer']
        xsi_outer = self.rel_vertices['xsi_outer']

        if self.device_type == 'flap':
            a_abs = get_abs_segment_point_coords(inner_seg_vertices, eta_inner, xsi_inner)
            b_abs = get_abs_segment_point_coords(outer_seg_vertices, eta_outer, xsi_outer)
            c_abs = get_abs_segment_point_coords(outer_seg_vertices, eta_outer, 1)
            d_abs = get_abs_segment_point_coords(inner_seg_vertices, eta_inner, 1)

        elif self.device_type == 'slat':
            a_abs = get_abs_segment_point_coords(inner_seg_vertices, eta_inner, 0)
            b_abs = get_abs_segment_point_coords(outer_seg_vertices, eta_outer, 0)
            c_abs = get_abs_segment_point_coords(outer_seg_vertices, eta_outer, xsi_outer)
            d_abs = get_abs_segment_point_coords(inner_seg_vertices, eta_inner, xsi_inner)

        return {'a': a_abs, 'b': b_abs, 'c': c_abs, 'd': d_abs}

    @property
    def abs_hinge_vertices(self):
        """Get absolute hinge coordinates of the control"""

        inner_seg_name = self.segment_uid['inner']
        outer_seg_name = self.segment_uid['outer']
        inner_seg_vertices = self.parent_wing.segments[inner_seg_name].vertices
        outer_seg_vertices = self.parent_wing.segments[outer_seg_name].vertices

        eta_inner = self.rel_vertices['eta_inner']
        eta_outer = self.rel_vertices['eta_outer']
        xsi_inner = self.rel_hinge_vertices['xsi_inner']
        xsi_outer = self.rel_hinge_vertices['xsi_outer']

        p1 = get_abs_segment_point_coords(inner_seg_vertices, eta_inner, xsi_inner)
        p2 = get_abs_segment_point_coords(outer_seg_vertices, eta_outer, xsi_outer)

        return {'p_inner': p1, 'p_outer': p2}

    def check(self):
        """Check definition of WINGCONTROL properties and data"""

        logger.info("Checking geometric properties...")

        self.SEGMENT_UID_SCHEMA.validate(self.segment_uid)
        self.PANELS_SCHEMA.validate(self.panels)
        self.REL_VERTICES_SCHEMA.validate(self.rel_vertices)
        self.REL_HINGE_VERTICES_SCHEMA.validate(self.rel_hinge_vertices)

        # ----- Check device type -----
        if self.device_type is None:
            raise ComponentDefinitionError("'device' is not defined.")

        if (self.segment_uid['inner'] == self.segment_uid['outer']) and \
                (self.rel_vertices['eta_outer'] <= self.rel_vertices['eta_inner']):
            raise ValueError("'eta_outer' must be greater than 'eta_inner'")

        # ----- Check deflection angle -----
        if self.deflection is None:
            raise ComponentDefinitionError("'deflection' is not defined.")
        elif not isinstance(self.deflection, (float, int)):
            raise TypeError("'deflection' must be FLOAT.")
        elif not -90.0 <= self.deflection <= +90.0:
            raise ValueError("'deflection' must be between -90.0 and +90.0 [deg].")
        else:
            self.deflection = float(self.deflection)

        if self.deflection_mirror is None:
            if self.symmetry != 0:
                logger.warning(f"Control '{self.uid:s}': 'deflection_mirror' is not set, " +
                               "but wing has symmetry. Will use 'deflection'")
                self.deflection_mirror = self.deflection
        elif self.symmetry == 0:
            logger.warning(f"Control '{self.uid:s}': 'deflection_mirror' is set, " +
                           "but wing has no symmetry. Value will be ignored.")
        elif not isinstance(self.deflection_mirror, (float, int)):
            raise TypeError("'deflection' must be FLOAT.")
        elif not -90.0 <= self.deflection_mirror <= +90.0:
            raise ValueError("'deflection' must be between -90.0 and +90.0 [deg].")
        else:
            self.deflection_mirror = float(self.deflection_mirror)


class SegmentStrip:
    """
    SegmentStrip is a "child class" of WingSegment.

    This object further divides each WingSegment into chordwise strips. This
    simplifies the meshing of the wing surface when there are leading and
    trailing edge devices. Subdivisions can also be geometrically transformed
    (translation and rotation). This makes it possible to perform a analyses on
    a deformed surface mesh, which is of particular interest for aeroelastic
    analyses.

    SegmentStrip are quadrilateral segments.

    Attributes:
        :segment: (obj) reference to the parent segment object
        :rel_vertices: (dict) relative coordinates of the subdivision defined by eta coordinates
        :subarea: (dict) dictionary containing subareas of a subdivision
    """

    def __init__(self, segment, rel_vertices):
        """
        Initialisation of the SegmentStrip

        Args:
            :segment: (obj) parent object (segment)
            :rel_vertices: (dict) relative coordinates of the subdivision (eta)
        """

        # Keep a reference about the parents outer vertices
        self.parent_segment = segment
        self.parent_wing = self.parent_segment.parent_wing

        # Relative vertices of the subdivision (w.r.t. segment)
        self.rel_vertices = {}
        self.rel_vertices['eta_a'] = rel_vertices['eta_a']
        self.rel_vertices['eta_b'] = rel_vertices['eta_b']
        self.rel_vertices['eta_c'] = rel_vertices['eta_c']
        self.rel_vertices['eta_d'] = rel_vertices['eta_d']

        # Subareas (by default always a 'segment' subarea)
        self.subarea = OrderedDict()
        subarea_segment_rel_vertices = {}
        subarea_segment_rel_vertices['xsi_a'] = 0.0
        subarea_segment_rel_vertices['xsi_b'] = 0.0
        subarea_segment_rel_vertices['xsi_c'] = 1.0
        subarea_segment_rel_vertices['xsi_d'] = 1.0
        self.subarea.update({'segment': StripSubdivision(self, subarea_segment_rel_vertices, subarea_type='segment')})

    @property
    def symmetry(self):
        """Symmetry inherited from parent wing"""

        return self.parent_wing.symmetry

    def abs_vertices(self, mirror):
        """
        Get absolute coordinates of the subdivision

        Absolute coordinates are only computed upon request based on the parent
        geometry (segment) and transformation directives if defined.

        Wing deformations are taken into account.

        Args:
            :mirror: (bool) flag to whether to return the mirrored or non-mirrored side

        Returns:
            :abs_vertices: (dict) absolute subdivision vertices
        """

        if self.parent_wing.is_deformed:
            eta_a = self.rel_vertices['eta_a']
            eta_b = self.rel_vertices['eta_b']
            eta_c = self.rel_vertices['eta_c']
            eta_d = self.rel_vertices['eta_d']

            a_abs = self.parent_segment.get_deformed_segment_point(eta_a, 0, mirror)
            b_abs = self.parent_segment.get_deformed_segment_point(eta_b, 0, mirror)
            c_abs = self.parent_segment.get_deformed_segment_point(eta_c, 1, mirror)
            d_abs = self.parent_segment.get_deformed_segment_point(eta_d, 1, mirror)

            if mirror:
                a_abs, b_abs, c_abs, d_abs = order_mirrored_vertex_points((a_abs, b_abs, c_abs, d_abs), self.symmetry)
        else:
            a = self.parent_segment.vertices['a']
            b = self.parent_segment.vertices['b']
            c = self.parent_segment.vertices['c']
            d = self.parent_segment.vertices['d']

            eta_a = self.rel_vertices['eta_a']
            eta_b = self.rel_vertices['eta_b']
            eta_c = self.rel_vertices['eta_c']
            eta_d = self.rel_vertices['eta_d']

            a_abs = a + eta_a*(b - a)
            b_abs = a + eta_b*(b - a)
            c_abs = d + eta_c*(c - d)
            d_abs = d + eta_d*(c - d)

            if mirror:
                a_abs, b_abs, c_abs, d_abs = mirror_vertices((a_abs, b_abs, c_abs, d_abs), self.symmetry)

        return {'a': a_abs, 'b': b_abs, 'c': c_abs, 'd': d_abs}

    def _add_subarea(self, parent_control, xsi1, xsi2, xsi_h1=None, xsi_h2=None):
        """
        Add a subarea to the current subdivision (intended for internal use only).

        Note:
            * If a subarea is added, the 'segment' subarea is updated
              automatically. The 'segment' subarea should NOT be modified manually.

        Args:
            :parent_control: (obj) reference to the parent control object
            :xsi1: (float) inner xsi position of the area division
            :xsi2: (float) outer xsi position of the area division
            :xsi_h1: (float) [optional] inner position of the hinge line (=xsi1 if not specified)
            :xsi_h2: (float) [optional] outer position of the hinge line (=xsi2 if not specified)

        Returns:
            :subarea: newly created subarea object
        """

        device_type = parent_control.device_type

        if device_type not in ['slat', 'flap']:
            raise ValueError(f"Unknown device type '{device_type}'")

        if device_type in self.subarea.keys():
            raise ValueError(f"Subdivision already has subarea for type '{device_type}'")

        if xsi_h1 is None:
            xsi_h1 = xsi1
            logger.warning("Hinge position xsi_h1 is not defined (assuming xsi1).")

        if xsi_h2 is None:
            xsi_h2 = xsi2
            logger.warning("Hinge position xsi_h2 is not defined (assuming xsi2).")

        for xsi in [xsi2, xsi1]:
            if (xsi <= 0 or xsi >= 1):
                raise ValueError(f"xsi must be in range (0, 1). Given xsi = {xsi:.2e}")

        for xsi in [xsi_h1, xsi_h2]:
            if (xsi < 0 or xsi > 1):
                raise ValueError(f"xsi_h must be in range [0, 1]. Given xsi = {xsi:.2e}")

        if 'slat' in self.subarea.keys():
            if self.subarea['slat'].rel_vertices['xsi_d'] + MIN_XSI_LIMIT > xsi1 \
                    or self.subarea['slat'].rel_vertices['xsi_c'] + MIN_XSI_LIMIT > xsi2:
                raise ValueError("Refusing to create overlapping subareas")

        if 'flap' in self.subarea.keys():
            if xsi1 + MIN_XSI_LIMIT > self.subarea['flap'].rel_vertices['xsi_a']  \
                    or xsi2 + MIN_XSI_LIMIT > self.subarea['flap'].rel_vertices['xsi_b']:
                raise ValueError("Refusing to create overlapping subareas")

        if device_type == 'slat':
            subarea_segment_rel_vertices = {}
            subarea_segment_rel_vertices['xsi_a'] = 0.0
            subarea_segment_rel_vertices['xsi_b'] = 0.0
            subarea_segment_rel_vertices['xsi_c'] = xsi1
            subarea_segment_rel_vertices['xsi_d'] = xsi2
            self.subarea.update({device_type: StripSubdivision(self,
                                subarea_segment_rel_vertices, subarea_type='slat')})

            self.subarea['segment'].rel_vertices['xsi_a'] = xsi1
            self.subarea['segment'].rel_vertices['xsi_b'] = xsi2

        elif device_type == 'flap':
            subarea_segment_rel_vertices = {}
            subarea_segment_rel_vertices['xsi_a'] = xsi1
            subarea_segment_rel_vertices['xsi_b'] = xsi2
            subarea_segment_rel_vertices['xsi_c'] = 1.0
            subarea_segment_rel_vertices['xsi_d'] = 1.0
            self.subarea.update({device_type: StripSubdivision(self,
                                subarea_segment_rel_vertices, subarea_type='flap')})

            self.subarea['segment'].rel_vertices['xsi_c'] = xsi2
            self.subarea['segment'].rel_vertices['xsi_d'] = xsi1

        # Update the relative hinge positions and add reference to parent control object
        self.subarea[device_type]._add_hinge_line(xsi_h1, xsi_h2)
        self.subarea[device_type]._add_parent_control(parent_control)

        return self.subarea[device_type]

    def _transform(self, le_translation, le_rotation):
        """
        Perform a coordinate transformation for the current subdivision.

        Args:
            :le_translation: (float) leading edge translation
            :le_rotation: (float) leading edge rotation
        """

        pass

    def _update_subarea(self, device_type, xsi1, xsi2, xsi_h1, xsi_h2):
        """
        Update an existing subarea and keep consistency with other subareas.

        Args:
            :device_type: (str) 'flap' or 'slat'
            :xsi1: (float) inner xsi position of the area division
            :xsi2: (float) outer xsi position of the area division
            :xsi_h1: (float) relative position of the inner hinge point
            :xsi_h2: (float) relative position of the outer hinge point
        """

        # TODO: add checks ??

        if device_type == 'slat':
            self.subarea['slat'].rel_vertices['xsi_c'] = xsi2
            self.subarea['slat'].rel_vertices['xsi_d'] = xsi1

            self.subarea['segment'].rel_vertices['xsi_a'] = xsi1
            self.subarea['segment'].rel_vertices['xsi_b'] = xsi2

            self.subarea['slat'].rel_hinge_vertices['xsi_h1'] = xsi_h1
            self.subarea['slat'].rel_hinge_vertices['xsi_h2'] = xsi_h2

        elif device_type == 'flap':
            self.subarea['flap'].rel_vertices['xsi_a'] = xsi1
            self.subarea['flap'].rel_vertices['xsi_b'] = xsi2

            self.subarea['segment'].rel_vertices['xsi_c'] = xsi2
            self.subarea['segment'].rel_vertices['xsi_d'] = xsi1

            self.subarea['flap'].rel_hinge_vertices['xsi_h1'] = xsi_h1
            self.subarea['flap'].rel_hinge_vertices['xsi_h2'] = xsi_h2


class StripSubdivision:
    """
    StripSubdivision is a "child class" of SegmentStrip.

    This object provides the basic geometric definition of subareas within a
    segment subdivision. Subareas are defined by their xsi coordinates within
    the subdivision. Absolute coordinates can be computed by calling the parent
    objects.

    Attributes:
        :_subdivision: (obj) reference to the parent subdivision object
        :rel_vertices: (dict) relative coordinates of the subarea defined by xsi coordinates
        :rel_hinge: (dict) relative coordinates of the hinge line (only for subarea of type {'flap', 'slat'})
        :parent_control: (obj) reference to the parent control object (only for subarea of type {'flap', 'slat'})
    """

    def __init__(self, subdivision, rel_vertices, subarea_type):
        """
        Initialise the StripSubdivision.

        Args:
            :subdivision: (obj) parent object (subdivision)
            :rel_vertices: (dict) relative coordinates of the subarea (xsi)
        """

        # Keep a reference about the parent object (subdivision)
        self.parent_subdivision = subdivision
        self.parent_segment = self.parent_subdivision.parent_segment
        self.parent_wing = self.parent_subdivision.parent_segment.parent_wing

        self.type = subarea_type

        # Relative position of vertices
        self.rel_vertices = {}
        self.rel_vertices['xsi_a'] = rel_vertices['xsi_a']
        self.rel_vertices['xsi_b'] = rel_vertices['xsi_b']
        self.rel_vertices['xsi_c'] = rel_vertices['xsi_d']
        self.rel_vertices['xsi_d'] = rel_vertices['xsi_c']

        # For subareas of type 'slat' and 'flap'
        # - Relative position of hinge line
        # - Parent control device
        self.rel_hinge = None
        self.parent_control = None

    @property
    def rel_length(self):
        """
        Return the average 'xsi length' of the subarea

        Returns:
            :xsi_avg: (float) average xsi length
        """

        xsi_a = self.rel_vertices['xsi_a']
        xsi_b = self.rel_vertices['xsi_b']
        xsi_c = self.rel_vertices['xsi_c']
        xsi_d = self.rel_vertices['xsi_d']

        return 0.5*((xsi_c + xsi_d) - (xsi_a + xsi_b))

    @property
    def symmetry(self):
        """
        Get symmetry property of subarea (inherited from parent wing).

        Returns:
            :symmetry: (int) symmetry property
        """

        return self.parent_wing.symmetry

    @property
    def segment_vertices(self):
        """
        Get segments coordinates of the parent segment.

        Returns:
            :vertices: (dict) dictionary segment vertices
        """

        return self.parent_subdivision.parent_segment.vertices

    def abs_vertices(self, mirror):
        """
        Get absolute coordinates of subarea vertices

        Wing deformations are taken into account.

        Args:
            :mirror: (bool) optional flag, if true mirror is returned

        Returns:
            :abs_vertices: (dict) dictionary with absolute coordinates of subarea vertices
        """

        subdivision_vertices = self.parent_subdivision.abs_vertices(mirror)

        a_sd = subdivision_vertices['a']
        b_sd = subdivision_vertices['b']
        c_sd = subdivision_vertices['c']
        d_sd = subdivision_vertices['d']

        xsi_a = self.rel_vertices['xsi_a']
        xsi_b = self.rel_vertices['xsi_b']
        xsi_c = self.rel_vertices['xsi_c']
        xsi_d = self.rel_vertices['xsi_d']

        if mirror:
            xsi_a, xsi_b, xsi_c, xsi_d = order_mirrored_vertex_points((xsi_a, xsi_b, xsi_c, xsi_d), self.symmetry)

        a_abs = a_sd + xsi_a*(d_sd - a_sd)
        b_abs = b_sd + xsi_b*(c_sd - b_sd)
        c_abs = b_sd + xsi_c*(c_sd - b_sd)
        d_abs = a_sd + xsi_d*(d_sd - a_sd)

        return {'a': a_abs, 'b': b_abs, 'c': c_abs, 'd': d_abs}

    def abs_hinge_vertices(self, mirror):
        """
        Get absolute coordinates of subarea hinge points.

        Wing deformations are taken into account.

        Args:
            :mirror: (bool) optional flag, if true mirror is returned

        Returns:
            :abs_hinge_vertices: (dict) dictionary with absolute hinge coordinates of subarea vertices
        """

        subdivision_vertices = self.parent_subdivision.abs_vertices(mirror)

        a_sd = subdivision_vertices['a']
        b_sd = subdivision_vertices['b']
        c_sd = subdivision_vertices['c']
        d_sd = subdivision_vertices['d']

        xsi_h1 = self.rel_hinge_vertices['xsi_h1']
        xsi_h2 = self.rel_hinge_vertices['xsi_h2']

        # Only in case of type 2 symmetry, the xsi coordinates should be swapped
        if mirror and self.symmetry == 2:
            xsi_h1, xsi_h2 = xsi_h2, xsi_h1

        p_inner = a_sd + xsi_h1*(d_sd - a_sd)
        p_outer = b_sd + xsi_h2*(c_sd - b_sd)

        return {'p_inner': p_inner, 'p_outer': p_outer}

    def abs_hinge_axis(self, mirror):
        """
        Get the hinge axis vector in the global coordinate system.

        Wing deformations are taken into account.

        Args:
            :mirror: (bool) optional flag, if true mirror axis is returned

        Returns:
            :abs_hinge_axis: absolute hinge axis in global system
        """

        hinge_vertices = self.abs_hinge_vertices(mirror)
        p_inner = hinge_vertices['p_inner']
        p_outer = hinge_vertices['p_outer']

        return p_outer - p_inner

    def abs_camber_line_rot_axis_vertices(self, mirror):
        """
        Get the rotation axis for the camber line.

        Note:
            * The rotation axis is defined by two points.

        Args:
            :mirror: (bool) optional flag, if true mirror axis is returned

        Returns:
            :abs_vertices: absolute axis vertices in global system
        """

        subarea_vertices = self.abs_vertices(mirror)

        a = subarea_vertices['a']
        b = subarea_vertices['b']
        d = subarea_vertices['d']

        ab = b - a
        ad = d - a

        subarea_normal = np.cross(ab, ad)
        rot_axis = np.cross(subarea_normal, ad)

        p_inner = a
        p_outer = a + rot_axis/np.linalg.norm(rot_axis)

        return {'p_inner': p_inner, 'p_outer': p_outer}

    def abs_camber_line_rot_axis(self, mirror):
        """
        Get the axis vector for camber line rotations in the global coordinate system.

        Wing deformations are taken into account.

        Args:
            :mirror: (bool) optional flag, if true mirror axis is returned

        Returns:
            :abs_axis: absolute axis in global system
        """

        axis_vertices = self.abs_camber_line_rot_axis_vertices(mirror)
        p_inner = axis_vertices['p_inner']
        p_outer = axis_vertices['p_outer']

        return p_outer - p_inner

    def _add_hinge_line(self, xsi_h1, xsi_h2):
        """
        Add a hinge to the subdivision.

        Args:
            :xsi_h1: (float) relative position of the inner hinge point
            :xsi_h2: (float) relative position of the outer hinge point
        """

        self.rel_hinge_vertices = {}
        self.rel_hinge_vertices['xsi_h1'] = xsi_h1
        self.rel_hinge_vertices['xsi_h2'] = xsi_h2

    def _add_parent_control(self, control_object):
        """
        Add a reference to the parent control device.

        Args:
            :control_object: (obj) control object to which the subarea belongs
        """

        # TODO: add tests ???

        self.parent_control = control_object

    def get_xsi_for_collocation_points(self, num_panels):
        """
        Return the relative xsi coordinates of a panel collocation points

        Note:
            * This functions assumes that all panels are linearly spaced!
            * Work for mirrored/non-mirrored subareas

        Args:
            :num_panels: number of panels on a segment subdivision

        Returns:
            :segment_xsi: list with xsi values of the collocation points
        """

        xsi_mid_ab = (self.rel_vertices['xsi_a'] + self.rel_vertices['xsi_b'])/2
        xsi_mid_cd = (self.rel_vertices['xsi_c'] + self.rel_vertices['xsi_d'])/2

        segment_xsi = []
        for i in range(num_panels):
            # Collocation points have a relative chordwise location of 0.75
            xsi = xsi_mid_ab + ((i + 0.75)/num_panels)*(xsi_mid_cd - xsi_mid_ab)
            segment_xsi.append(xsi)

        return segment_xsi


def get_abs_segment_point_coords(segment_vertices, eta, xsi):
    """
    Compute the absolute coordinates for a point on a segment

    Warning:
        * The function does only work for UNDEFORMED segments!

    Args:
        :segment_vertices: (dict) segment vertices
        :rel_coords: (dict) relative coordinates (eta, xsi)

    Returns:
        :p: (ndarray) absolute coordinates
    """

    a = segment_vertices['a']
    b = segment_vertices['b']
    c = segment_vertices['c']
    d = segment_vertices['d']

    a_eta = a + eta*(b - a)
    d_eta = d + eta*(c - d)

    return a_eta + xsi*(d_eta - a_eta)


def xsi_interpol(segment_vertices, rel_inner, rel_outer, eta):
    """
    Return the linear interpolation of xsi value based on relative segment coordinates

    Args:
        :segment_vertices: (dict) segment vertices
        :rel_inner: (tuple) eta and xsi coordinates of the inner position
        :rel_outer: (tuple) eta and xsi coordinates of the outer position
        :eta: (float) eta position at which xsi is to be interpolated

    Return:
        :xsi_interpol: (float) interpolated value for xsi
    """

    eta_inner, xsi_inner = rel_inner
    eta_outer, xsi_outer = rel_outer

    p_inner = get_abs_segment_point_coords(segment_vertices, eta_inner, xsi_inner)
    p_outer = get_abs_segment_point_coords(segment_vertices, eta_outer, xsi_outer)
    insegment_direction = p_inner - p_outer

    p_upper = get_abs_segment_point_coords(segment_vertices, eta, 0)
    p_lower = get_abs_segment_point_coords(segment_vertices, eta, 1)

    a = segment_vertices['a']
    b = segment_vertices['b']
    d = segment_vertices['d']

    segment_normal = np.cross(b-a, d-a)
    intersect_plane_normal = np.cross(segment_normal, p_lower-p_upper)

    p_intersect = get_plane_line_intersect(intersect_plane_normal, p_upper, insegment_direction, p_inner)
    xsi_interpol = np.linalg.norm(p_intersect-p_upper)/np.linalg.norm(p_lower-p_upper)

    return xsi_interpol


def mirror_point(point, plane):
    """
    Mirror a point in 3D space about a symmetry plane.

    Args:
        :point: point
        :plane: (str) plane ('xy', 'xz' or 'yz')
    """

    point = copy(point)

    if plane == 'xy' or plane == 1:
        point[2] = -point[2]
    elif plane == 'xz' or plane == 2:
        point[1] = -point[1]
    elif plane == 'yz' or plane == 3:
        point[0] = -point[0]
    else:
        raise ValueError(f"Invalid plane (plane: '{plane}')")

    return point


def order_mirrored_vertex_points(vertices, plane):
    """
    Order mirrored vertex points to keep consistent global system

    Args:
        :vertices: (tuple) vertices like (a, b, c, d)
        :plane: (str) plane ('xy', 'xz' or 'yz')

    Returns:
        :ordered_vertices: (tuple) ordered vertices
    """

    a, b, c, d = vertices

    if plane == 'xy' or plane == 1:
        pass
    elif plane == 'xz' or plane == 2:
        a, b, c, d = b, a, d, c
    elif plane == 'yz' or plane == 3:
        a, b, c, d = d, c, b, a
    else:
        raise ValueError(f"Invalid plane (plane: '{plane}')")

    return (a, b, c, d)


def mirror_vertices(vertices, plane):
    """
    Mirror vertices and keep vertex points consistently ordered.

    This is a wrapper combining the mirroring and ordering.

    Args:
        :vertices: (tuple) vertices like (a, b, c, d)
        :plane: (str) plane ('xy', 'xz' or 'yz')

    Returns:
        :ordered_vertices: (tuple) ordered vertices
    """

    a, b, c, d = vertices

    a = mirror_point(a, plane)
    b = mirror_point(b, plane)
    c = mirror_point(c, plane)
    d = mirror_point(d, plane)

    return order_mirrored_vertex_points((a, b, c, d), plane)
