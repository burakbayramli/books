# 
# Copyright (c) 2009-2023 fem2ufo
#

# Python stdlib imports
from __future__ import annotations
from array import array
from collections.abc import Mapping
import re
from typing import NamedTuple #, Tuple, List, Iterator, Dict, ClassVar


# package imports
from steelpy.metocean.hydrodynamic.operations import BasicProperty, get_list
import numpy as np

#
class Direction(NamedTuple):
    """
    Direction dependent coefficients
    """
    x:float
    y:float
    z:float    
#
class CdCm(NamedTuple):
    """
    Morison parameters CD & Cm
    """
    #Cdx:float
    #Cdy:float
    Cd:float
    #
    #Cmx:float
    #Cmy:float
    Cm:float
    #
    #number:int
    #sets:List
    #case:str
    #
    @property
    def drag(self):
        return Direction(self.Cdx, self.Cdy, self.Cdz)
    
    @property
    def mass(self):
        return Direction(self.Cmx, self.Cmy, self.Cmz)  
#
#
#
#
# ---------------------------------------------
#
#
class CdCmCoefficients(Mapping):
    """
    """
    __slots__ =  ['_cdcm']
    
    def __init__(self):
        """
        """
        self._cdcm: dict = {}
    
    def __getitem__(self, cdcm_name):
        """
        """
        return self._cdcm[cdcm_name]
    
    def __setitem__(self, name, values:list|tuple|dict|str) -> None:
        """
        rule
        specified
        diametre
        """
        cdcm_type = values.pop(0)
        #values = self._get_value(values)
        if re.match(r"\b((rule(\_)?)(api|iso))\b", cdcm_type, re.IGNORECASE):
            self._cdcm[name] = DiametreFunction()
            self._cdcm[cdcm_name].rule = cdcm_type
        elif re.match(r"\b(specified|coefficients)\b", cdcm_type, re.IGNORECASE):
            self._cdcm[name] = SpecifiedFunction()
            self._cdcm[name].set_cdcm(cdcm=values)
        elif re.match(r"\b(diamet(re|re))\b", cdcm_type, re.IGNORECASE):
            self._cdcm[name] = DiametreFunction()
        elif re.match(r"\b(depth(\_)?profile)\b", cdcm_type, re.IGNORECASE):
            self._cdcm[name] = DepthProfileFunction()
        else:
            raise IOError("CdCm type {:} not implemented".format(cdcm_type))         
    #
    def _get_value(self, value:list|tuple|dict|str):
        """ """
        if isinstance(value, (list, tuple)):
            # [name, cdx, cdy, cdz, cmx, cmy, cmz]
            value = get_list(value, steps)
        elif isinstance(value, dict):
            value = get_dic(value)
        else:
            raise Exception('   *** input format not recognized')
        return value

    #
    #
    def __len__(self) -> float:
        return len(self._cdcm)

    def __iter__(self) -> Iterator[Tuple]:
        return iter(self._cdcm)
    
    def __contains__(self, value) -> bool:
        return value in self._cdcm
    #
    #
    #
    def getCdCm(self, Z, HTs: float, condition:int):
        """ """
        cm = np.zeros((Z.shape))
        cm += 1.2
        cm[Z > HTs] = 1.6
        #cm[Z <= 2] = 1.2
        #
        cd = np.zeros((Z.shape))
        # switch condition
        if condition == 1:
            cd += 1.15
        else:
            #elif condition == 2:
            cd += 1.05
            cd[Z > HTs] = 0.65
            #cd[Z <= 2] = 1.05
        #
        return cd, cm
    #
#
#
class DiametreFunction(BasicProperty):
    __slots__ = ['_type', '_diameter', '_rule']
    
    def __init__(self):
        """
        """
        BasicProperty.__init__(self)
        self._type = 'diameter_function'
        self._diameter:Dict = {}
    
    def diameter_function(self, diameter, smooth, rough):
        """
        """
        self._diameter[diameter] = [smooth, rough]
    #
    @property
    def rule(self):
        """
        """
        return self._rule
    
    @rule.setter
    def rule(self, value:str):
        """
        """
        self._rule = value
#
class SpecifiedFunction(BasicProperty):
    __slots__ = ['_type', '_coefficients']
    
    def __init__(self):
        """
        """
        BasicProperty.__init__(self)
        self._type = 'specified_function'
    
    #@property
    def set_cdcm(self, cdcm:list|dict):
        """
        """
        if type(cdcm) == list:
            self._coefficients = CdCm._make(cdcm)
        elif type(cdcm) == dict:
            self._coefficients = CdCm.Fixity(**cdcm)
        else:
            self._coefficients = CdCm(Cd, Cm)
#
class DepthProfileFunction(BasicProperty):
    __slots__ = ['_type', '_profile']
    
    def __init__(self):
        """
        """
        BasicProperty.__init__(self)
        self._type = 'depth_profile'
        self._profile:List = []
    #
    #
    def depth_profile(self, depth, cdcm):
        """
        """
        #_coefficient = MorrisonCoefficient.set_cdcm(self, cdcm)
        self._profile.extend([depth, cdcm])
   