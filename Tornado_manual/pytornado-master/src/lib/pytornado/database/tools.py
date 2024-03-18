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
PyTornado database tools

Developed for Airinnova AB, Stockholm, Sweden.
"""

from pathlib import Path
import glob
import logging
import os
import json

logger = logging.getLogger(__name__)

PATH_AIRCRAFT_DB = 'aircraft'


def get_aircraft_db_path():
    """
    Return the absolute path of the aircraft database

    Returns:
        :path_aircraft_db: (str) absolute path
    """

    path_aircraft_db = os.path.join(
        os.path.abspath(os.path.dirname(__file__)),
        PATH_AIRCRAFT_DB
    )
    return path_aircraft_db


def list_full_aircraft_paths():
    """
    List aircraft files in the database

    Returns:
        :aircraft: (list) Aircraft paths
    """

    path_aircraft = os.path.join(get_aircraft_db_path(), '*.json')
    return glob.glob(path_aircraft, recursive=False)


def list_aircraft_names():
    """
    Return a list of available aircraft in the database (short names)

    Returns:
        :names_aircraft: (list) List with names of aircraft
    """

    paths_aircraft = list_full_aircraft_paths()
    names_aircraft = []
    for path in paths_aircraft:
        names_aircraft.append(Path(Path(path).name).stem)
    return names_aircraft


def get_aircraft_file_path(aircraft_name):
    """
    Return the full aircraft file path for given short name

    Args:
        :aircraft_name: (str) Short name of the aircraft

    Returns:
        :path: (str) Full file path (or None if file not found)

    Raises:
        :FileNotFoundError: If no corresponding file can be located
    """

    paths_aircraft = list_full_aircraft_paths()
    for path in paths_aircraft:
        if Path(Path(path).name).stem == aircraft_name:
            return path
    raise FileNotFoundError


def print_available_aircraft():
    """
    Print a list of available aircraft to stdout
    """

    print("Available aircraft in database:\n")
    for aircraft_name in list_aircraft_names():
        file_aircraft = get_aircraft_file_path(aircraft_name)
        with open(file_aircraft) as fp:
            model_aircraft = json.load(fp)
        comment = model_aircraft.get('comment', '[no information]')
        print(f"* {aircraft_name:10s} -- {comment}")
