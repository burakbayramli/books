# 
# Copyright (c) 2009-2023 fem2ufo
#

# Python stdlib imports
from __future__ import annotations
#from dataclasses import dataclass
#from typing import NamedTuple, Tuple, List, Iterator, Dict, Iterable


# package imports
#from steelpy.process.units.main import Units
from .codecheck.codecheck import CodeCheck



#
#
class Properties:
    
    __slots__ = ['_hydrodynamic', '_code_check']
    
    def __init__(self):
        """
        """
        #self._units = Units()
        self._code_check = CodeCheck()
    #
    #@property
    #def units(self):
    #    """
    #    """
    #    return self._units    
    #
    #
    #@property
    def design_parameters(self):
        """ """
        return self._code_check
#
    