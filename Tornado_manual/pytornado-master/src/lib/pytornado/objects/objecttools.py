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
Generators to facilitate looping over aircraft objects

Developed for Airinnova AB, Stockholm, Sweden.
"""


def all_wings(aircraft):
    """
    Yield each wing of the aircraft.

    Args:
        :aircraft: aircraft object

    Yields:
        :this_wing: tuple containing the wing number, name and object
    """

    for wing_no, wing_data in enumerate(aircraft.wings.items()):
        wing_uid, wing_obj = wing_data
        yield (wing_no, wing_uid, wing_obj)


def all_segments(aircraft):
    """
    Yield each segment of the aircraft.

    Args:
        :aircraft: aircraft object

    Yields:
        :this_segment: tuple containing the segment number, name and object
        :this_wing: tuple containing the wing number, name and object
    """

    for this_wing in all_wings(aircraft):
        for segment_no, segment_data in enumerate(this_wing[2].segments.items()):
            segment_uid, segment_obj = segment_data
            yield (segment_no, segment_uid, segment_obj), this_wing


def all_controls(aircraft):
    """
    Yield each control surface of the aircraft.

    Args:
        :aircraft: aircraft object

    Yields:
        :this_control: tuple containing the control surface number, name and object
        :this_wing: tuple containing the wing number, name and object
    """

    for this_wing in all_wings(aircraft):
        for control_no, control_data in enumerate(this_wing[2].controls.items()):
            control_uid, control_obj = control_data
            yield (control_no, control_uid, control_obj), this_wing


def all_subdivisions(aircraft):
    """
    Yield each subdivision of the aircraft.

    Args:
        :aircraft: aircraft object

    Yields:
        :this_subdivision: tuple containing the subdivision number, name and object
        :this_segment: tuple containing the segment number, name and object
        :this_wing: tuple containing the wing number, name and object
    """

    for this_segment, this_wing in all_segments(aircraft):
        for subdivision_no, subdivision_data in enumerate(this_segment[2].subdivision.items()):
            subdivision_name, subdivision_obj = subdivision_data
            yield (subdivision_no, subdivision_name, subdivision_obj), this_segment, this_wing


def all_subareas(aircraft):
    """
    Yield each subarea of the aircraft.

    Args:
        :aircraft: aircraft object

    Yields:
        :this_subarea: tuple containing the subarea number, name and object
        :this_subdivision: tuple containing the subdivision number, name and object
        :this_segment: tuple containing the segment number, name and object
        :this_wing: tuple containing the wing number, name and object
    """

    for this_subdivision, this_segment, this_wing in all_subdivisions(aircraft):
        for subarea_no, subarea_data in enumerate(this_subdivision[2].subarea.items()):
            subarea_name, subarea_obj = subarea_data
            yield (subarea_no, subarea_name, subarea_obj), this_subdivision, this_segment, this_wing


def all_subareas_of_control(aircraft, control_uid):
    """
    Yield each subarea belonging to a control of specified name.

    Args:
        :aircraft: aircraft object
        :control_uid: name (UID) of the control

    Yields:
        :this_subarea: tuple containing the subarea number, name and object
    """

    for this_subarea, _, _, _ in all_subareas(aircraft):
        if this_subarea[2].parent_control.uid == control_uid:
            yield this_subarea


def count_all_wings(aircraft):
    """
    Returns the number of wings of the aircraft.

    Args:
        :aircraft: aircraft object

    Returns:
        :n: number of wings
    """

    n = 0
    for _ in all_wings(aircraft):
        n += 1
    return n


def count_all_segments(aircraft):
    """
    Returns the number of segments of the aircraft.

    Args:
        :aircraft: aircraft object

    Returns:
        :n: number of segments
    """

    n = 0
    for _, _ in all_segments(aircraft):
        n += 1
    return n


def count_all_controls(aircraft):
    """
    Returns the number of control surfaces of the aircraft.

    Args:
        :aircraft: aircraft object

    Returns:
        :n: number of control surfaces
    """

    n = 0
    for _, _ in all_controls(aircraft):
        n += 1
    return n


def count_all_subareas(aircraft):
    """
    Returns the number of subareas of the aircraft.

    Args:
        :aircraft: aircraft object

    Returns:
        :n: number of subareas
    """

    n = 0
    for _, _, _, _ in all_subareas(aircraft):
        n += 1
    return n
