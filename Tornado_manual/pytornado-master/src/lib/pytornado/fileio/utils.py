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
Utils for reading and writing files

Developed at Airinnova AB, Stockholm, Sweden.
"""

from functools import partial
import json
import numpy as np


class NDArrayEncoder(json.JSONEncoder):
    """
    Serialise numpy arrays

    Use with: json.dump(obj, fp, cls=NDArrayEncoder)
    """

    def default(self, obj):
        if isinstance(obj, np.ndarray):
            # Covert to list (numpy method)
            return obj.tolist()
        return json.JSONEncoder.default(self, obj)


# Dump pretty-formatted JSON with support for numpy arrays
dump_pretty_json = partial(json.dump, cls=NDArrayEncoder, indent=4, separators=(',', ': '))


def parse_str(entry, allow_char='-_'):
    """
    Strip undesired characters from a string

    Note:
        * Alphanumeric characters always accepted: [a...z], [A...Z], [0...9]

    Args:
        :entry: (string) Input string
        :allow_char: (string) Additional accepted characters
        :allow_none: (bool) Detect when input is None
        :allow_bool: (bool) Detect when input is True or False

    Returns:
        :parsed_string: Parsed string
    """

    if not isinstance(entry, str):
        raise ValueError("Input is not of type 'string'")

    parsed_string = ''.join([c for c in entry if c.isalnum() or c in allow_char])
    return parsed_string
