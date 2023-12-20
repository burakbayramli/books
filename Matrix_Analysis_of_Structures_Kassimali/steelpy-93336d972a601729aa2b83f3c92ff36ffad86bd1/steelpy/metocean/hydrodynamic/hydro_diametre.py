# 
# Copyright (c) 2009-2023 fem2ufo
#

# Python stdlib imports
from __future__ import annotations
from array import array
#import logging
from typing import NamedTuple #, Tuple, List, Iterator, Dict, Union
from collections.abc import Mapping

# package imports
from steelpy.metocean.hydrodynamic.operations import BasicProperty


#
# TODO: remove this
class HydroDiam(BasicProperty):
    
    __slots__ = ('diameter', 'case')
    
    def __init__(self) -> None:
        """
        """
        BasicProperty.__init__(self)
        self.diameter = []
        self.case = None
    #
    def diameter_function(self, diameter, smooth, rough):
        """
        """
        if self.case:
            self.diameter[diameter] = [smooth, rough]
        else:
            self.case = 'diameter_function'
            self.diameter = {}
            self.diameter[diameter] = [smooth, rough]
#
class Hdiameter(NamedTuple):
    diameter:float
    name:str
    group:Tuple
#
#
class HydroDiametre(Mapping):
    """
    """
    __slots__ = ('_label', '_diameter', '_title', 'sets')
    
    def __init__(self) -> None:
        """
        """
        self._label : array = array('I', [])
        self._title : list[float|int|str] = []
        self._diameter : array = array('f', [])
        self.sets : dict = {}
    
    def __getitem__(self, parameter_name):
        """
        """
        try:
            index = self._title.index(parameter_name)
            return Hdiameter(self._diameter[index], parameter_name, 
                             self.sets[parameter_name])
        except ValueError:
            raise KeyError(' hydrodynamic diameter {:} not found'.format(parameter_name))
    
    def __setitem__(self, parameter_name, value:float) -> None:
        """
        """
        self._title.append(parameter_name)
        self._diameter.append(value)
        _number = len(self._title)
        self._label.append(_number)
    #
    def __delitem__(self, parameter_name) -> None:
        """
        """
        try:
            index = self._title.index(parameter_name)
            self._title.pop(index)
            self._diameter.pop(index)
            del self.sets[parameter_name]
        except IndexError:
            logging.warning(' ** node {:} does not exist'.format(node_number))
            return
    
    def __len__(self) -> float:
        return len(self._title)

    
    def __iter__(self):
        return iter(self._title)
    
    def __contains__(self, value) -> bool:
        return value in self._title    
#