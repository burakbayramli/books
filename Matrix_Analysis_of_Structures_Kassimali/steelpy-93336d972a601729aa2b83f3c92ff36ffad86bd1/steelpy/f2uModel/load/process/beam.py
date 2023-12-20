#
# Copyright (c) 2009-2023 fem2ufo
# 

# Python stdlib imports
from __future__ import annotations
from array import array
#from dataclasses import dataclass
from collections.abc import Mapping
#from typing import NamedTuple
#import re


# package imports


# ---------------------------------
#
class BeamDistMaster(Mapping):
    
    def __init__(self) -> None:
        """
        """
        self._index: int
        self._labels: list[str|int] = []
        self._title: list[str] = []
        self._load_id: list[str|int] = []
        self._complex: array = array("I", [])
        # 0-global/ 1-local
        #self._system_flag: int = 0
        self._system: array = array("I", [])
    #
    def __len__(self) -> int:
        return len(self._labels)
    #
    def __contains__(self, value) -> bool:
        return value in self._labels
    #
    def __iter__(self):
        """
        """
        items = list(set(self._labels))
        return iter(items)
    #
    def __str__(self) -> str:
        """ """
        output = ""
        beams = list(dict.fromkeys(self._labels))
        #beams = list(set(self._labels))
        for beam in beams:
            items = self.__getitem__(beam)
            for item in items:
                output += item.__str__()
        #print('---')
        return output
    #
    #
    def _get_line_load(self):
        """ return line load in correct format"""
        print('-->')
        1/0
    #
    #
    #
    #
    #@property
    #def coordinate_system(self):
    #    if self._system_flag != 0:
    #        return "local"
    #    return "global"
    #
    #@coordinate_system.setter
    #def coordinate_system(self, system:str|int):
    #    """
    #    Coordinate system for load : global or local (member)
    #    """
    #    self._system_flag = 0
    #    if system in ['local', 'member', 1]:
    #        self._system_flag = 1    
#
#