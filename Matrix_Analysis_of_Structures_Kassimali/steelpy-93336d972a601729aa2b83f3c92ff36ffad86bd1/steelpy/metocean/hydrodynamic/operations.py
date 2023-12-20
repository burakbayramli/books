# 
# Copyright (c) 2009-2023 fem2ufo
#
# Python stdlib imports
from __future__ import annotations
#from array import array
#import logging
#from typing import  List, Union, Tuple # Iterator, Dict, Union, NamedTuple,
#from collections.abc import Mapping

# package imports
#from steelpy.process.units.units import Units


class BasicProperty:
    __slots__ = ['name', '_groups', '_elements']
    
    def __init__(self):
        """
        """
        #self._units = Units()
        self._groups:List = []
        self._elements:List = []
        #self._default:bool = False
    
    #
    @property
    def group(self):
        """
        """
        return self._groups
    
    @group.setter
    def group(self, value:list|tuple|str|int):
        """
        """
        if isinstance(value, (list, tuple)):
            self._groups.extend(list(value))
        else:
            self._groups.append(value)
    #
    #
    @property
    def elements(self):
        """
        """
        return self._elements
    
    @elements.setter
    def elements(self, value:list|tuple|str|int):
        """
        """
        if isinstance(value, (list, tuple)):
            self._elements.extend(list(value))
        else:
            self._elements.append(value)
    #
    #def set_default(self):
    #    """
    #    """
    #    self._default = True
#
#
#
def get_list(data, steps:int=6)->list[float]:
    """ """
    new_data = []
    for x in range(steps):
        try:
            try:
                new_data.append(data[x].value)
            except AttributeError:
                new_data.append(data[x])
        except IndexError:
            new_data.append(0.0)
    return new_data
#