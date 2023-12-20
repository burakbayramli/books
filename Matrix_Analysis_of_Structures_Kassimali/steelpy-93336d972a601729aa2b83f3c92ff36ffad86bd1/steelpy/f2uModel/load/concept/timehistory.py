#
# Copyright (c) 2009-2023 steelpy
#
# Python stdlib imports
from __future__ import annotations
from collections.abc import Mapping
from dataclasses import dataclass

#
# package imports
# steelpy.f2uModel.load
from ..process.combination import LoadCombinationBasic
from ..concept.combination import CombTypes
#


#
class THtypes(CombTypes):
    """
    """
    __slots__ = ['_points', '_metocean', '_transition', '_sine', 
                 'name', 'title'] #'number',
    
    def __init__(self, name: str|int, title:str):
        """
        """
        super().__init__(name, title)
        #
        self._points = {} #THload("points")
        self._transition = {}
    #
    @property
    def points(self):
        """
        dtime : The constant time increment between tabulated values [None]
        time : list[1, 2, n]
        factor: list[1, 2, n]
        """
        return self._points[self.name]
    
    @points.setter
    def points(self, values):
        """
        dtime : The constant time increment between tabulated values [None]
        time : list[1, 2, n]
        factor: list[1, 2, n]
        """
        time = [item.convert('second').value for item in values[0]]
        self._points[self.name] = PointLoad(time=time, factor=values[1])
    #
    def transition(self):
        """
        time : list[1, 2, 3]
        factor: list[1, 2, 3]
        power : [linear, 2order, 3order]
        """
        return self._transition[self.name]
    
    def transition(self):
        """
        time : list[1, 2, 3]
        factor: list[1, 2, 3]
        power : [linear, 2order, 3order]
        """
        time = [item.convert('second').value for item in values[0]]
        self._transition[self.name] = PointLoad(time=time, factor=values[1])        
    #
    def sine(self):
        """
        amplitud:
        period:
        phase:
        tstart: Start time
        nperiod: Number of period
        """
        pass
    #
    def metocean(self):
        """
        dtime : The constant time increment between tabulated values [None]
        factor : metocean forces multiplied by factor [1.0]
        tstart: Start time
        """
        pass

#
#
class TimeHistory(LoadCombinationBasic):
    __slots__ = ('name', '_combination', '_basic')
    
    def __init__(self):
        """
        """
        super().__init__()
        #self._load: dict = {}
        self._combination = {}
    #
    def __setitem__(self, load_name:int, load_title:str) -> None:
        """
        load_name :
        load_title :
        """
        try:
            self._labels.index(load_name)
            self._title.index(load_title)
            raise Warning("TH Load title {:} already defined".format(load_title))
        except ValueError:
            self._labels.append(load_name)
            self._title.append(load_title)
                        #
            self._combination[load_name] = THtypes(name=load_name, title=load_title)
            # TODO: fix numbering sequence
            load_number =  len(self._combination)
            self._combination[load_name].number = load_number
            self._number.append(load_number)
    #
    def __getitem__(self, load_name:str|int):
        """
        """
        return self._combination[load_name]
    #
    def __delitem__(self, load_name:str|int):
        """
        """
        del self._combination[load_name]
#
#
#
class THload(Mapping):
    __slots__ = ['_basic', '_type']

    def __init__(self, bl_type:str):
        """
        """
        self._basic:Dict = {}
        self._type = bl_type
    #
    def __setitem__(self, load_name: str|int, factor: float) -> None:
        """
        """
        self._basic[load_name] = factor
    #
    def __getitem__(self, load_name:str|int):
        """
        """
        return self._basic[load_name]
    #
    #
    def __delitem__(self, load_name:str|int):
        """
        """
        del self._basic[load_name]
    #
    def __len__(self) -> int:
        return len(self._basic)
    #
    def __iter__(self):
        """
        """
        return iter(self._basic)
    #
    #
    @property
    def load_type(self):
        """
        """
        return self._type    
#
#
@dataclass
class PointLoad:
    """
    """
    time: list
    factor: list
    dtime: float | None = None
#
#