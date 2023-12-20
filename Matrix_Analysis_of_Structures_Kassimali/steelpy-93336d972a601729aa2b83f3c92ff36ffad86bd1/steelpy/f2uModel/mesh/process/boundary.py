# 
# Copyright (c) 2009-2023 fem2ufo
#
# Python stdlib imports
from __future__ import annotations
#from array import array
from collections.abc import Mapping
import re
from typing import NamedTuple


#
# -----------------------
# TODO: merge with slite
class BoundaryItem(NamedTuple):
    """
    """
    x: float
    y: float
    z: float
    rx: float
    ry: float
    rz: float
    number: int
    name: str|None
    node:int
    
    def __str__(self) -> str:
        if (name := self.name) == None:
            name = ""
        return "{:12d} {: 8.0f} {: 8.0f} {: 8.0f} {: 8.0f} {: 8.0f} {: 8.0f} {:>12s}\n"\
            .format(self.node, self.x, self.y, self.z, self.rx, self.ry, self.rz, name)
#
#
class BoundaryNode(Mapping):
    #__slots__ = ['db_file', '_labels']
    
    def __init__(self):
        """
        """
        self._labels: list[int|str] = []
    #
    #
    def __len__(self) -> float:
        return len(self._labels)

    def __iter__(self):
        """
        """
        return iter(self._labels)

    def __contains__(self, value) -> bool:
        return value in self._labels
    #
    # ----------------------------
    # Operations
    # ----------------------------
    #
    def get_boundary(self, name:str):
        """ """
        if re.match(r"\b(fix(ed)?|encastre)\b", name, re.IGNORECASE):
            #self._title.append('fixed')
            value = [1,1,1,1,1,1]
        elif re.match(r"\b(pinn(ed)?|roll)\b", name, re.IGNORECASE):
            #self._title.append('pinned')
            value = [1,1,1,0,0,0]
        elif re.match(r"\b(free)\b", name, re.IGNORECASE):
            value = [0,0,0,0,0,0]
        else:
            raise IOError("boundary type {:} not implemented".format(name))
        #
        return value
    #
    def _get_fixity(self, fixity):
        """ """
        if isinstance(fixity, str):
            if re.match(r"\b(fix(ed)?)\b", fixity, re.IGNORECASE):
                return [1,1,1,1,1,1]
            elif re.match(r"\b(pinn(ed)?|roll)\b", fixity, re.IGNORECASE):
                return [1,1,1,0,0,0]
            elif re.match(r"\b(free)\b", fixity, re.IGNORECASE):
                return None
            else:
                raise IOError("boundary type {:} not implemented".format(fixity))
        elif isinstance(fixity, (list, tuple)):
            return fixity
        elif isinstance(fixity, dict):
            return [fixity['x'], fixity['y'], fixity['z'], 
                    fixity['rx'], fixity['ry'], fixity['rz']]
        else:
            raise Exception('   *** Boundary input format not recognized')
    #
#    