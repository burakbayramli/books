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
Miscellaneous data structures for PyTornado.

Developed for Airinnova AB, Stockholm, Sweden.
"""


from collections import OrderedDict
from collections.abc import MutableMapping


def check_dict(template_dict, test_dict):
    """
    Check that a test dictionary looks like a template dictionary

    Args:
        :template_dict: Template dictionary
        :test_dict: Test dictionary

    The template dictionary must have a specific structure as outlined below:

    .. code:: python

        template_dict = {
            'test_key1': ('default_value1', str),
            'test_key2': (1792, (int, float)),
        }

    The values have to be tuples with some default value and the expected
    types of the values. The follwing dictionary would pass the test:

    .. code:: python

        test_dict = {
            'test_key1': 'some_string',
            'test_key2': 1111,
        }

    However, the follwing dictionary does not have the correct form and an
    error will be raised.

    .. code:: python

        test_dict = {
            'test_key1': 'this is okay...',
            'test_key2': '... but a string is now allowed here',
        }

    Raises:
        :TypeError: If types of test and template dictionary don't match
    """

    # TODO: handle None better!!!

    for key, (value, dtype) in template_dict.items():
        dtype = (dtype,) if not isinstance(dtype, tuple) else dtype

        if None not in dtype:
            if not isinstance(test_dict[key], dtype):
                err_msg = f"""
                Unexpected data type for key '{key}'.
                Expected {dtype}, got {type(test_dict[key])}.
                """
                raise TypeError(err_msg)

        if dtype[0] is dict:
            check_dict(value, test_dict[key])


def get_default_dict(template_dict):
    """
    Return a default dict from a template dictionary

    Args:
        :template_dict: Template dictionary

    Returns:
        :default_dict: New dictionary with defaults generated from 'template_dict'

    The template dictionary must have a specific structure as outlined below:

    .. code:: python

        template_dict = {
            'test_key1': ('default_value1', str),
            'test_key2': (1792, (int, float)),
        }

    The 'default_dict' will look like this:

    .. code:: python

        default_dict = {
            'test_key1': 'default_value1',
            'test_key2': 1792,
        }
    """

    default_dict = {}
    for key, (value, _) in template_dict.items():
        # Treat non-empty dictionary recursively
        if isinstance(value, dict) and value:
            value = get_default_dict(template_dict=value)
        default_dict[key] = value
    return default_dict
