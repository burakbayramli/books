# 
# Copyright (c) 2019-2023 steelpy
#

# Python stdlib imports
from __future__ import annotations
from array import array
from dataclasses import dataclass
from typing import NamedTuple

# package imports
#
#from steelpy.process.units.main import Units
from steelpy.utils.io_module.text import match_line
# steelpy.f2uModel.load.process
from .operations import (check_list_units, 
                         check_list_number, 
                         check_point_dic)

#
#
class PointGrav(NamedTuple):
    """
    """
    fx: float
    fy: float
    fz: float
    number: int | str
    load_name: str
    load_type:str = "gravity"
    #
    def __str__(self, units: str = "si") -> str:
        """ """
        if (load_name := self.load_name) == "NULL":
            load_name = ""
        #
        coordinate_system = "global"
        output = (f"{str(self.number):12s} {10 * ' '} "
                  f"{self.fx: 1.3e} {self.fy: 1.3e} {self.fz: 1.3e} "
                  f"{coordinate_system.upper():6s} "
                  f"{load_name:<16s}\n")
        #
        #step = self.load_type
        #output += (f"{step:12s} {10 * ' '} "
        #           f"{self.mx: 1.3e} {self.my: 1.3e} {self.mz: 1.3e} {self.load_complex}\n")
        return output    
#
#
@dataclass
class SelfWeight:
    """ """
    __slots__ = [ '_fx', '_fy', '_fz', '_labels', '_title']

    def __init__(self):
        """ """
        self._fx: array  = array('f',[])
        self._fy: array  = array('f',[])
        self._fz: array  = array('f',[])
        #
        self._labels: list[Union[str, int]] = []
        self._title: list[Union[str, int, None]] = []        
    #
    def __setitem__(self, load_name: int|str, gravity: list) -> None:
        """
        load_name :
        load_title :
        """
        try:
            self._labels[load_name]
            raise IOError("Gravity already defined in Load Case:{:}".format(load_name))
        except IndexError:
            self._labels.append(load_name)
            if isinstance(gravity[-1], str):
                title = gravity.pop()
                self._title.append(title)
                #gravity = get_gravity_load(gravity)
            else:
                self._title.append("NULL")
                #gravity = get_gravity_load(gravity)
            #
            gravity = get_gravity_load(gravity)
            self._fx.append(gravity[0])
            self._fy.append(gravity[1])
            self._fz.append(gravity[2])
            #self._index = len(self._labels) - 1
    #
    def __getitem__(self, load_name: int|str):
        """
        """
        index = self._labels.index(load_name)
        return PointGrav(self._fx[index], self._fy[index], self._fz[index],
                         self._labels[index], self._title[index])
    #
    def __iter__(self):
        """
        """
        #items = list(dict.fromkeys(self._labels))
        return iter(self._labels)

    def __contains__(self, value) -> bool:
        return value in self._labels

    def __len__(self) -> float:
        return len(self._labels)    
    #
    def __str__(self) -> str:
        """ """
        output = ""
        #nodes = list(dict.fromkeys(self._labels))
        for node in self._labels:
            item = self.__getitem__(node)
            output += item.__str__()
        return output    
#
#
#
def get_gravity_load(load, steps: int=3):
    """ """
    if isinstance(load, (list, tuple)):
        try:
            load = check_list_units(load)
            load = load[:steps]
        except AttributeError:
            load = check_list_number(load, steps=steps)
    elif isinstance(load, dict):
        1 / 0
        #load = check_point_dic(load)
    else:
        raise Exception('   *** Load input format not recognized')
    #
    new_load = []
    for item in load:
        try:
            new_load.append(item.convert('metre/second^2').value)
        except AttributeError:
            new_load.append(item)
    #
    return new_load
#
#
#
def find_force_item(word_in):
    """
    """
    _key = {"Fx": r"\b((f|axial|p)(\_)?(x|1)?)\b",
            "Fy": r"\b((f|v|shear)(\_)?(y|i(n)?|2(\_)?(p(lane)?)?))\b",
            "Fz": r"\b((f|v|shear)(\_)?(z|o(ut)?|3(\_)?(p(lane)?)?))\b",
            #
            "Mx": r"\b((bending(\_)?)?m(oment)?(\_)?(x|t(orsion(al)?)?|1))\b",
            "My": r"\b((bending(\_)?)?m(oment)?(\_)?(y|i(n)?(\_)?p(lane)?|2))\b",
            "Mz": r"\b((bending(\_)?)?m(oment)?(\_)?(z|o(ut)?(\_)?p(lane)?|3))\b"}
    
    _match = match_line(word_in, _key)
    if not _match:
        raise IOError('  ** item {:} not recognized'.format(word_in))
    return _match
#
# FIXME: badly done to assign forces
def assign_force_item(self, mat_items):
    """
    Assign material's input data to a dictionary by name/number
    """
    #
    #if not self.actions:
    #    self.actions = Actions(Fx = 0 * self._units.N, 
    #                           Fy = 0 * self._units.N, 
    #                           Fz = 0 * self._units.N, 
    #                           Mx = 0 * self._units.N * self._units.m, 
    #                           My = 0 * self._units.N * self._units.m, 
    #                           Mz = 0 * self._units.N * self._units.m)
    #
    for key, value in mat_items.items():
        _item = find_force_item(key)
        #
        if _item == 'Fx':
            self.Fx = value
        elif _item == 'Fy':
            self.Fy = value        
        elif _item == 'Fz':
            self.Fz = value
        #
        elif _item == 'Mx':
            self.Mx = value        
        elif _item == 'My':
            self.My = value
        elif _item == 'Mz':
            self.Mz = value
        else:
            raise IOError('error Force item : {:} not recognized'
                          .format(key))
    #
    #self.actions = Actions(_P, _Vy, _Vx, _Mt, _Mx, _My)
    #print('ok')
#
#
class Actions:
    """
    Force & bending moments
    """
    __slots__ = ['_Fx', '_Fy', '_Fz', '_Mx', '_My', '_Mz']
    
    def __init__(self):
        """
        """
        #self._units = Units()
        self._Fx = 0 #* self._units.N
        self._Fy = 0 #* self._units.N
        self._Fz = 0 #* self._units.N
        self._Mx = 0 #* self._units.N * self._units.m
        self._My = 0 #* self._units.N * self._units.m
        self._Mz = 0 #* self._units.N * self._units.m
        #
        # ----- Load offsets -----
        #self.Xe = 0 * units.m # load offset from support 1
    
    #@property
    #def units(self):
    #    """
    #    units [length, mass, time, temperature, force, pressure/stress]/n
    #    """
    #    return self._units    
    #
    def __setattr__(self, name, value):
        """ """
        if name=="device":
            print("device test")
        else:
            super().__setattr__(name, value)
    #
    #def __setitem__(self, **kwargs):
    #    """
    #    """
    #    print('here')
    def member_actions(self, **kwargs):
        """
        """
        assign_force_item(self, kwargs)
    #
    #
    #def __str__(self, units:str="si") -> str:
    #    """ """
    #    output  = (f"{str(self.number):12s} {self.fx: 1.3e} "
    #               f"{self.fy: 1.3e} {self.fy: 1.3e}\n")
    #    step = 12*" "
    #    output += (f"{step} {10*' '} {self.mx: 1.3e} "
    #               f"{self.my: 1.3e} {self.mz: 1.3e}\n")
    #    return output
#