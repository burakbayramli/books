# 
# Copyright (c) 2009-2023 fem2ufo
#

# Python stdlib imports
from __future__ import annotations
#from array import array
#import copy
#from dataclasses import dataclass
#import logging
from typing import NamedTuple #, Tuple, List, Iterator, Dict
from collections.abc import Mapping

# package imports

# TODO: update this
class Flooding(Mapping):
    __slots__ = ('_flooded')
    #_flooded:Dict = {}
    
    def __init__(self) -> None:
        """
        """
        self._flooded: dict = {}
    
    
    def __getitem__(self, flood_name: str) -> bool:
        """
        """
        return self._flooded[flood_name]
    
    def __setitem__(self, flood_name: str, flood_status:bool) -> None:
        """
        """
        self._flooded[flood_name] = flood_status
    #
    def __len__(self) -> float:
        return len(self._flooded)

    
    def __iter__(self) -> Iterator[Tuple]:
        return iter(self._flooded)
    
    def __contains__(self, value) -> bool:
        return value in self._flooded     
#