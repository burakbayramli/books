# 
# Copyright (c) 2009-2023 steelpy
#
from __future__ import annotations
# Python stdlib imports
import re

#
# package imports

#
# ====================================================================
#
def match_line(word_in, key):
    """
    search word at the begining of the string
    """
    word_out=None
    for _key, _item in key.items():
        rgx = re.compile(_item, re.IGNORECASE)
        keys = rgx.match(word_in)

        if keys:
            word_out = _key
    #
    return word_out
#
# ====================================================================
#
def select_items(files):
    """
    """
    _number = []
    _name = []

    for _item in files:
        if type(_item) is list:
            for _memb in _item:
                _str, _int = get_items(_memb)
                _name.extend(_str)
                _number.extend(_int)
        else:
            _str, _int = get_items(_item)
            _name.extend(_str)
            _number.extend(_int)
    #
    return _name, _number
#
def get_items(_item):
    """
    """
    _str = []
    _int = []

    if type(_item) is str:
        _str.append(_item)

    elif type(_item) is int:
        _int.append(_item)

    else:
        print('   ** warining item {:} not recognized'
              .format(_item))

    return _str, _int
#
# ====================================================================
#
def search_line(line_in, key, key_word=None, count=1):
    """
    search key word anywere in the the string
    """
    lineOut = line_in
    _match = False
    for _key, _item in key.items():
        rgx = re.compile(_item, re.IGNORECASE)
        keys = rgx.search(line_in)

        if keys:
            key_word = _key
            lineOut = re.sub(keys.group(), " ", line_in, count)
            _match = True

    lineOut = lineOut.strip()
    return key_word, lineOut, _match
#
def match_keywords(line_in, key, key_word=None, count=1):
    """
    search key word at the begining of the string
    """
    line_out = line_in
    _match = False
    for _key, _item in key.items():
        rgx = re.compile(_item, re.IGNORECASE)
        keys = rgx.match(line_in)

        if keys:
            key_word = _key
            line_out = re.sub(keys.group(), " ", line_in, count)
            _match = True
    #
    line_out = line_out.strip()
    return key_word, line_out, _match
#
# ====================================================================
#
def update_key_word(key_word, ind, value=None):
    """
    """
    for x in range(ind + 1, len(key_word)):
        key_word[x] = value
    # return key_word
#
#
def find_keyword(word_in, keys):
    """
    Find keyword data from user
    """
    _match = match_line(word_in, keys)

    if not _match:
        raise IOError('  **  error data {:} not recognized'.format(word_in))
        #print('      process terminated')
        #1/0
        #sys.exit()
    return _match
#