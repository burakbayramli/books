#
# Copyright (c) 2009-2023 fem2ufo
# 

# Python stdlib imports
from __future__ import annotations
#from array import array
#from collections.abc import Mapping
#from collections import defaultdict
#from dataclasses import dataclass
#from operator import sub, add
from typing import NamedTuple
# import re
#
# package imports
#import pandas as pd
# steelpy
from steelpy.trave.beam.load.beam import BeamLoadItem
#from ....process.math.operations import zeros, trns_3Dv #, zeros_vector , linspace
#from ....process.math.vector import Vector
#from steelpy.formulas.beam.main import BasicCalcs
#


#
#
class WaveData(NamedTuple):
    """ """
    name: str | int
    seastate: dict
    design_load: str
    criterion : str
#
#@dataclass
class WaveLoadItem(BeamLoadItem):
    """ """
    __slots__ = ['_seastate', '_name',
                 '_load', '_criterion', '_design_load']

    def __init__(self, load_name: int|str):
        """ """
        super().__init__()
        #
        self._name = load_name        
        #
        self._seastate:list = []
        self._design_load:list = []
        self._criterion:list = []
    #
    def __setitem__(self, load_name: int|str,
                    wave_load: list) -> None:
        """
        criterion : 
        """
        try:
            self._labels.index(load_name)
            raise Exception('wave load case {:} already exist'.format(load_name))
        except ValueError:
            self._labels.append(load_name)
            self._seastate.append(wave_load[0])
            self._design_load.append(wave_load[1])
            self._criterion.append(wave_load[2])
    #
    def __getitem__(self, load_name: int | str):
        """
        """
        try:
            index = self._labels.index(load_name)
        except ValueError:
            raise KeyError('Invalid load name : {:}'.format(load_name))
        #
        return WaveData(name=self._labels[index], 
                        seastate=self._seastate[index],
                        design_load=self._design_load[index],
                        criterion=self._criterion[index])        
    #