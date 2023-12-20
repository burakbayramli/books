#
# Copyright (c) 2009 steelpy
# 

# Python stdlib imports
from __future__ import annotations
from array import array
from typing import NamedTuple
from collections.abc import Mapping

# package imports
#import pandas as pd
# steelpy.f2uModel.load.process
from .operations import (check_list_units,
                         check_list_number,
                         check_point_dic)

from steelpy.utils.dataframe.main import DBframework
# from steelpy.f2uModel.load.sqlite.node import NodeLoadSQL

#
#
class PointNode(NamedTuple):
    """
    [fx, fy, fz, mx, my, mz, name, title, load_name, system, load_complex, load_type]
    """
    fx: float
    fy: float
    fz: float
    mx: float
    my: float
    mz: float
    name: int|str
    title: str
    load_name: int|str
    system: int
    load_complex: int
    load_type:str = "Nodal Load"

    #
    def __str__(self, units: str = "si") -> str:
        """ """
        output = (f"{str(self.name):12s} {10 * ' '} "
                  f"{self.fx: 1.3e} {self.fy: 1.3e} {self.fz: 1.3e} "
                  f"{self.coordinate_system.upper():6s} "
                  f"{self.load_complex}\n")
                  
        #
        if (load_title := self.title) == "NULL":
            load_title = ""        
        step = self.load_type
        output += (f"{step:12s} {10 * ' '} "
                   f"{self.mx: 1.3e} {self.my: 1.3e} {self.mz: 1.3e} "
                   f"{str(load_title)}\n")
        return output

    #
    @property
    def coordinate_system(self) -> str:
        if self.system != 0:
            return "local"
        return "global"
    #


#
class NodeLoadBasic(Mapping):
    """
    """
    #__slots__ = ['_nodes']

    def __init__(self) -> None:
        """
        """
        self._labels: list = []
        self._title: list = []
        self._complex: array = array("I", [])
        self._load_id: list = []
        #self._number: array = array("I", [])
        # 0-global/ 1-local
        #self._system_flag: int = 0
        self._system: array = array("I", [])

    def __iter__(self):
        """
        """
        items = list(dict.fromkeys(self._labels))
        #items = set(self._labels)
        return iter(items)

    def __contains__(self, value) -> bool:
        return value in self._labels

    def __len__(self) -> int:
        return len(self._labels)

    #
    def __str__(self) -> str:
        """ """
        output = ""
        nodes = list(dict.fromkeys(self._labels))
        #nodes = set(self._labels)
        for node in nodes:
            items = self.__getitem__(node)
            for item in items:
                output += item.__str__()
                # print('---')
        return output
    #
    #
    def _get_point_load(self, point_load:str|dict):
        """ return point load in correct format"""
        #print('-->')
        if isinstance(point_load, dict):
            point_load = get_nodal_load(point_load)
            self._title.append(point_load[-1])
            point_load.pop()
        elif isinstance(point_load[-1], str):
            title = point_load.pop()
            self._title.append(title)
            point_load = get_nodal_load(point_load)
        else:
            try:
                point_load[6]
                self._title.append(point_load.pop())
            except IndexError:
                self._title.append("NULL")
            point_load = get_nodal_load(point_load)
        1/0
    #
    #

#
#
class NodeLoadMaster(NodeLoadBasic):
    """
    FE Node Load class
    
    NodeLoad
        |_ name
        |_ number
        |_ type
        |_ complex
        |_ point [x, y, z, mx, my, mz]
        |_ acceleration [th[0], th[1], th[2],..., th[n]]
        |_ displacement [th[0], th[1], th[2],..., th[n]]
        |_ mass
    
    **Parameters**:  
      :number:  integer internal number 
      :name:  string node external name
    """
    #__slots__ = ['_title', '_labels', '_index', '_complex',
    #             '_fx', '_fy', '_fz', '_mx', '_my', '_mz',
    #             '_system', '_system_flag'] #, '_distance'

    def __init__(self, load_type:str) -> None:
        """
        """
        super().__init__()
        # real
        self._type = load_type
        self._fx: array = array('f', [])
        self._fy: array = array('f', [])
        self._fz: array = array('f', [])
        self._mx: array = array('f', [])
        self._my: array = array('f', [])
        self._mz: array = array('f', [])
        #self._distance: array = array('f', [])
    #
    #
    def __getitem__(self, node_name: int|str) -> list:
        """
        """
        _index_list: list = [x for x, _item in enumerate(self._labels)
                             if _item == node_name]
        #
        _points: list = []
        for _index in _index_list:
            _points.append(PointNode(self._fx[_index], self._fy[_index], self._fz[_index],
                                     self._mx[_index], self._my[_index], self._mz[_index],
                                     self._labels[_index], self._title[_index], self._load_id[_index],
                                     self._system[_index], self._complex[_index], self._type))
        return _points
    #
    #
    def __delitem__(self, node_name: int|str) -> None:
        """
        """
        indexes = [i for i, x in enumerate(self._labels)
                   if x == node_name]
        indexes.sort(reverse=True)
        for _index in indexes:
            self._fx.pop(_index)
            self._fy.pop(_index)
            self._fz.pop(_index)
            self._mx.pop(_index)
            self._my.pop(_index)
            self._mz.pop(_index)
            #
            self._labels.pop(_index)
            self._title.pop(_index)
            self._system.pop(_index)
            #self._distance.pop(_index)
            self._complex.pop(_index)
    #
    #
    #
    #@property
    #def coordinate_system(self):
    #    if self._system_flag != 0:
    #        return "local"
    #    return "global"
    #
    #@coordinate_system.setter
    #def coordinate_system(self, system:str|int):
    #    """
    #    Coordinate system for load : global or local (member)
    #    """
    #    self._system_flag = 0
    #    if system in ['local', 'member', 1]:
    #        self._system_flag = 1
    #
    #
    @property
    def df(self):
        """nodes in dataframe format"""
        db = DBframework()
        # TODO : merge nodes
        #title = []
        data = {'load_name': self._load_id,
                'load_type': ['basic' for item in self._labels],
                'load_number': [idx + 1 for idx, item in enumerate(self._labels)],
                'load_system': self._system, #['global' if item == 0 else 'local'
                           #for item in self._system],
                'load_comment': self._title,
                'node_name':self._labels, 
                'Fx':self._fx, 'Fy':self._fy, 'Fz':self._fz, 
                'Mx':self._mx, 'My':self._my, 'Mz':self._mz}
        #
        # beam point case
        try:
            data.update({'L1': self._L1})
        except AttributeError:
            pass
        #
        # beam node load conversion
        try:
            data.update({'element_name': self._beam})
        except AttributeError:
            pass
        #      
        #
        df_nload = db.DataFrame(data=data, index=None)
        #df = df[['load_name', 'load_type', 'load_number', 'load_system', 'load_comment',
        #         'node_name', 'Fx', 'Fy', 'Fz', 'Mx', 'My', 'Mz']]          
        return df_nload

    @df.setter
    def df(self, values):
        """nodes in dataframe format"""
        #update
        self._labels.extend(values.node_name.tolist())
        self._load_id.extend(values.load_name.tolist())
        self._title.extend(values.load_title.tolist())
        self._system.extend([1 if item in ['local', 'member', 1] else 0
                             for item in values.system.tolist()])
        self._complex.extend([0 for item in values.system.tolist()])
        #
        self._fx.extend(values.Fx.tolist())
        self._fy.extend(values.Fy.tolist())
        self._fz.extend(values.Fz.tolist())
        self._mx.extend(values.Mx.tolist())
        self._my.extend(values.My.tolist())
        self._mz.extend(values.Mz.tolist())
        #
        # beam point case
        try:
            self._L1.extend(values.L1.tolist())
        except AttributeError:
            pass
        #
        # beam node load conversion
        try:
            self._beam.extend(values.element_name.tolist())
        except AttributeError:
            pass
        #
        #print('nodes df out')
    
#
#
def get_nodal_load(load, steps: int = 6):
    """ """
    if isinstance(load, (list, tuple)):
        try:
            load = check_list_units(load)
            load = load[:steps]
        except AttributeError:
            load = check_list_number(load, steps=steps)
    elif isinstance(load, dict):
        load = check_point_dic(load)
    else:
        raise Exception('   *** Load input format not recognized')
    return load
#
#
#
class NodeForce(NamedTuple):
    """
    """
    fx: float
    fy: float
    fz: float
    mx: float
    my: float
    mz: float
    name: int|str
    load_name: str
    system:str
    load_complex:int
    #
    def __str__(self, units:str="si") -> str:
        """ """
        output  = (f"{str(self.name):12s} {10*' '} "
                   f"{self.fx: 1.3e} {self.fy: 1.3e} {self.fy: 1.3e}"
                   f"{0: 1.3e} {0: 1.3e} {0: 1.3e}\n")
        #step = 12*" "
        output += (f"{self.coordinate_system.upper():12s} {10*' '} "
                   f"{self.mx: 1.3e} {self.my: 1.3e} {self.mz: 1.3e}\n")
        return output
    #
    @property
    def coordinate_system(self):
        if self.system != 0:
            return "local"
        return "global" 
#
#
#
#
class NodeItem:
    #__slots__ = ['_load', '_displacement', '_mass', '_node_id']

    def __init__(self):
        """
        """
        pass
    #
    def __call__(self, node_id):
        self._node_id = node_id
        return self
    #
    #
    @property
    def load(self):
        """
        """
        try:
            point_id = self._node_id
            return self._load[point_id]
        except :
            raise IndexError

    @load.setter
    def load(self, values: list):
        """
        Point Load
        """
        node_name = self._node_id
        if isinstance(values, dict):
            self._load[node_name] = values
        elif isinstance(values[0], list):
            for value in values:
                self._load[node_name] = value
        else:
            self._load[node_name] = values

    #
    @property
    def mass(self):
        """
        """
        point_id = self._node_id
        return self._mass[point_id]

    @mass.setter
    def mass(self, values: list):
        """
        """
        node_name = self._node_id
        if isinstance(values[0], list):
            for value in values:
                self._mass[node_name] = value
        else:
            self._mass[node_name] = values

    #
    @property
    def displacement(self):
        """
        """
        point_id = self._node_id
        return self._displacement[point_id]

    @displacement.setter
    def displacement(self, values: list):
        """
        """
        node_name = self._node_id
        if isinstance(values[0], list):
            for value in values:
                self._displacement[node_name] = value[1:]
        else:
            self._displacement[node_name] = values

    #   
    #
    def __str__(self, units: str = "si") -> str:
        """ """
        output = ""
        # output += "--- Nodal Load \n"
        output += self._load.__str__()
        output += self._mass.__str__()
        output += self._displacement.__str__()
        return output