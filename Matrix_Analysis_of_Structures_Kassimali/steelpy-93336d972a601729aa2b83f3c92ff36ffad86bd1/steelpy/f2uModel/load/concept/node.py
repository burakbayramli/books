#
# Copyright (c) 2009-2023 fem2ufo
#
# Python stdlib imports
from __future__ import annotations
from collections.abc import Mapping
import re
#
# package imports
# steelpy.f2uModel.load
from ..process.nodes import (NodeLoadMaster, NodeItem,
                             get_nodal_load, PointNode)


# ---------------------------------
#
class NodeLoadItemIM(Mapping):
    __slots__ = ['_title', '_labels', '_type', '_number',
                 '_f2u_nodes', #'_load_name',
                 '_load', '_displacement', '_mass', '_node']

    def __init__(self, load_name, load_title, nodes) -> None:
        """
        """
        self._labels = []
        self._type = []
        #self._load_name = load_name
        #self._number = []
        #
        self._node = NodeItemIM(load_name=load_name,
                                    load_title=load_title)        
        self._f2u_nodes = nodes
    #
    def __setitem__(self, node_name: int|str,
                    node_load: list) -> None:
        """
        """
        load_type = node_load[0]
        #load_number = next(self.get_number())
        self._labels.append(node_name)
        #
        if re.match(r"\b(point|load|node)\b", load_type, re.IGNORECASE):
            #self.node_load._load = node_load[1:]
            self._node._load[node_name] = node_load[1:]
        
        elif re.match(r"\b(mass)\b", load_type, re.IGNORECASE):
            self._node._mass[node_name] = node_load[1:]
        
        elif re.match(r"\b(disp(lacement)?)\b", load_type, re.IGNORECASE):
            self._node._displacement[node_name] = node_load[1:]
        
        else:
            raise IOError(f'node load type {load_type} not recognized')
        #
        #self._labels.append(node_number)
        #self._type.append(load_type)
        #self._number.append(load_number)
    #
    def __getitem__(self, node_number: int | str):
        """
        """
        try:
            self._f2u_nodes[node_number]
            if not node_number in self._labels:
                self._labels.append(node_number)
            #
            return self._node(node_number)
        except KeyError:
            raise IOError(f"Node {node_number} not found")

    #
    def __contains__(self, value) -> bool:
        return value in self._labels

    def __len__(self) -> int:
        return len(self._labels)

    def __iter__(self):
        """
        """
        items = list(dict.fromkeys(self._labels))
        #items = list(set(self._labels))
        return iter(items)

    #
    def __str__(self, units: str = "si") -> str:
        """ """
        output = ""
        output += self._node.__str__()
        return output
    #
    #
    #def get_number(self, start:int=1):
    #    """
    #    """
    #    try:
    #        n = max(self._number) + 1
    #    except ValueError:
    #        n = start
    #    #
    #    while True:
    #        yield n
    #        n += 1
    #
    @property
    def load(self):
        """
        """
        return self._node._load
    #
    #@load.setter
    #def load(self, value):
    #    """
    #    """
    #    self._load

#
class NodeItemIM(NodeItem):
    __slots__ = ['_load', #'_load_name',
                 '_displacement', '_mass', '_node_id']

    def __init__(self, load_name:str|int, load_title:str):
        """
        """
        super().__init__()
        #self._load_name = load_name
        self._load = NodeLoadIM(load_name, load_title, "load")
        self._displacement = NodeLoadIM(load_name, load_title, "displacement")
        self._mass = NodeLoadIM(load_name, load_title, "mass")
    #
#
#
class NodeLoadIM(NodeLoadMaster):
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
    __slots__ = ['_title', '_labels', '_index', '_complex',
                 '_fx', '_fy', '_fz', '_mx', '_my', '_mz',
                 '_system', '_system_flag', '_load_name', '_load_title']

    def __init__(self, load_name:str|int, load_title:str, 
                 load_type:str) -> None:
        """
        """
        super().__init__(load_type)
        self._load_name = load_name
        self._load_title = load_title
        self._system_flag: int = 0

    #
    def __setitem__(self, node_name: int|str,
                    point_load: list) -> None:
        """
        """
        #
        self._labels.append(node_name)
        #self._index = len(self._labels) - 1
        self._load_id.append(self._load_name)
        #
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
        #
        self._system.append(self._system_flag)
        #self._distance.append(-1)
        self._complex.append(0)
        #
        # point_load = get_nodal_load(point_load)
        self._fx.append(point_load[0])
        self._fy.append(point_load[1])
        self._fz.append(point_load[2])
        self._mx.append(point_load[3])
        self._my.append(point_load[4])
        self._mz.append(point_load[5])
        #
        # print('--')
    #
    # @property
    # def get_items_sum(self):
    def get_group_nodes(self):
        """
        """
        items = []
        set_items = set(self._labels)
        for item in set_items:
            _index = self._labels.index(item)
            rows = self.__getitem__(item)
            sum_load = [0, 0, 0, 0, 0, 0]
            for _load in rows:
                sum_load[0] += _load.fx
                sum_load[1] += _load.fy
                sum_load[2] += _load.fz
                sum_load[3] += _load.mx
                sum_load[4] += _load.my
                sum_load[5] += _load.mz
            items.append(PointNode(*sum_load,
                                   self._labels[_index],
                                   self._title[_index],
                                   self._system[_index],
                                   self._complex[_index]))
        return items
    #
    # def update(self, other) -> None:
    #    """
    #    """
    #    #1/0
    #    pl = other._point
    #    try:
    #        #_name = pl.name
    #        index = pl._index
    #        # update
    #        self._labels.extend(pl._labels)
    #        self._title.extend(pl._title)
    #        self._system.extend(pl._system)
    #        self._distance.extend(pl._distance)
    #        self._complex.extend(pl._complex)
    #        #
    #        self._fx.extend(pl._fx)
    #        self._fy.extend(pl._fy)
    #        self._fz.extend(pl._fz)
    #        self._mx.extend(pl._mx)
    #        self._my.extend(pl._my)
    #        self._mz.extend(pl._mz)
    #        # print('?????')
    #    except AttributeError:
    #        pass
    #
#
