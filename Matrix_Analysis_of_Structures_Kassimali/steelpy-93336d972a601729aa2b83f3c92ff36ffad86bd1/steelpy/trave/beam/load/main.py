#
# Copyright (c) 2009-2023 fem2ufo
# 

# Python stdlib imports
from __future__ import annotations
from array import array
from dataclasses import dataclass
from collections.abc import Mapping
#import re

# package imports
from steelpy.f2uModel.load.concept.beam import BeamLoadIM
from steelpy.f2uModel.load.process.actions import SelfWeight
#from steelpy.f2uModel.load.inmemory.combination import BasicLoad
#
class BeamBasicLoad:
    """
    FE Load Cases

    LoadType
        |_ name
        |_ number
        |_ basic
        |_ combination_level
        |_ time_series
        |_
        |_ temperature

    **Parameters**:
      :number:  integer internal number
      :name:  string node external name
    """
    __slots__ = ['_cls',  '_selfweight', #'gravity',
                 '_basic', 'combination']

    def __init__(self, beam):
        """
        """
        self._cls = beam
        #self._system_flag = 1 # local
        #self.gravity = 9.80665  # m/s^2
        #
        self._selfweight = SelfWeight()
        self._basic = BeamLoadIM("beam_load", "basic")
        self._basic.local_system()
        self.combination = BeamLoadComb("beam_load")
    #
    @property
    def basic(self):
        """return basic beam load"""
        return self._basic(beam=self._cls)
    #
    #@property
    #def combination(self):
    #    """return beam load combinations"""
    #    return self._combinations

    def __str__(self, units: str = "si") -> str:
        """ """
        unit_lenght = " m"
        unit_force = "  N"
        unit_bm = "N*m"
        unit_fl = "N/m"
        output = "\n"
        output += "{:}\n".format(80 * "_")
        output += "\n"
        output += f"{35 * ' '}BASIC LOAD\n"
        output += "\n"
        output += f"--- Beam \n"
        output += f"Element Name{6 * ' '}L1[{unit_lenght}] qx1[{unit_fl}] qy1[{unit_fl}] qz1[{unit_fl}] System Complex\n"
        output += f"Line Load{9 * ' '}L2[{unit_lenght}] qx2[{unit_fl}] qy2[{unit_fl}] qz2[{unit_fl}] Comment\n"
        output += "\n"
        output += f"--- Beam \n"
        output += f"Element Name{6 * ' '}L1[{unit_lenght}] fx [{unit_force}] fy [{unit_force}] fz [{unit_force}] System Complex\n"
        output += f"Point Load{15 * ' '}mx [{unit_bm}] my [{unit_bm}] mz [{unit_bm}] Comment\n"
        output += "\n"
        output += f"--- Gravity/Selfweight\n"
        #output += f"Element Number{4 * ' '}L1[{unit_lenght}] fx [{unit_force}] fy [{unit_force}] fz [{unit_force}] System Complex\n"
        output += "\n"
        output += "{:}\n".format(80 * ".")
        output += "\n"
        output += f"--- Beam \n"
        output += self._basic.__str__()
        output += "\n"
        return output
#
#
@dataclass
class BeamLoadComb:
    
    def __init__(self, b):
        """
        """
        self._labels = []
        self._factor: array = array("f", [])
    #
    def __setitem__(self, load_name: int|str,
                    factor: float) -> None:
        """ """
        
        self._labels.append(load_name)
        self._factor.append(factor)
    
    def __getitem__(self, element_name: int|str) :
        """
        """
        pass
#
#
class BeamBasicItem(Mapping):
    
    def __init__(self, beam_name:str):
        """
        """
        self._labels = {}
    
    
    def __iter__(self):
        """
        """
        items = list(dict.fromkeys(self._labels))
        return iter(items)

    def __contains__(self, value) -> bool:
        return value in self._labels

    def __len__(self) -> int:
        return len(self._labels)    