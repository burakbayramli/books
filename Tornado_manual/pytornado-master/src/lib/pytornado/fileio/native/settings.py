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
Functions for reading and writing of PyTornado execution settings file.

Developed at Airinnova AB, Stockholm, Sweden.
"""

import logging
import json
from pathlib import Path

from commonlibs.logger import truncate_filepath

from pytornado.objects.settings import Settings
from pytornado.fileio.utils import dump_pretty_json

logger = logging.getLogger(__name__)


def load(settings_filepath):
    """
    Read PyTornado settings from PyTornado settings file.

    Args:
        :settings: (object) data structure for execution settings
    """

    settings_filepath = Path(settings_filepath).resolve()
    if not settings_filepath.exists:
        raise IOError(f"File '{settings_filepath}' not found")

    settings_filename = settings_filepath.name
    wkdir = settings_filepath.parent

    # ==================================
    # TODO: make more general!!!!
    if str(wkdir.name) == 'settings':
        wkdir = wkdir.parent
    # ==================================

    with open(settings_filepath, 'r') as fp:
        settings_dict = json.load(fp)

    settings = Settings(settings_filename, wkdir, settings_dict=settings_dict)
    return settings


def save(settings):
    """
    Write settings to native settings file

    Args:
        :settings: (object) data structure for execution settings
    """

    set_file = settings.paths('f_settings')
    logger.info(f"Saving settings to file '{truncate_filepath(set_file)}'...")

    clean_dict(settings.settings)

    with open(set_file, 'w') as fp:
        dump_pretty_json(settings.settings, fp)


def clean_dict(dictionary, to_del='_'):
    """
    Delete dictionary items with keys starting with specified string

    Works recursively

    Args:
        :dictionary: Dictionary object
        :to_del: Starts-with identifier
    """

    to_delete = []
    for k, v in dictionary.items():
        if isinstance(v, dict):
            v = clean_dict(v)
        if k.startswith('_'):
            to_delete.append(k)

    for k in to_delete:
        del dictionary[k]
    return dictionary
