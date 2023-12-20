#
# Copyright (c) 2009-2023 steelpy
#
# Python stdlib imports
from __future__ import annotations
#
#
# package imports
# steelpy.f2uModel.load
from ..process.actions import SelfWeight
#from ..inmemory.beam import BeamLoadItemIM
#from ..inmemory.node import NodeLoadItemIM
from ..process.basic_load import BasicLoadBasic, LoadTypeBasic

#from steelpy.f2uModel.load.inmemory.wave_load import WaveLoadItemIM

#
#
# ---------------------------------
#
#
#
class BasicLoad(BasicLoadBasic):
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
    __slots__ = ['_labels', '_title','_number', 'gravity',
                 '_basic', '_f2u_elements', '_f2u_nodes']

    def __init__(self, nodes, elements):
        """
        """
        super().__init__()
        self._basic: dict = {}
        self._f2u_elements = elements
        self._f2u_nodes = nodes
    #
    def __setitem__(self, load_name: str|int, load_title: str) -> None:
        """
        load_name :
        load_title :
        """
        1 / 0
        try:
            self._labels.index(load_name)
            self._title.index(load_title)
            raise Warning("Basic Load {:} already defined".format(load_title))
        except ValueError:
            self._labels.append(load_name)
            self._title.append(load_title)
            load_number = next(self.get_number())
            self._basic[load_name] = LoadTypeInMemory(name=load_name,
                                                      number=load_number,
                                                      title=load_title,
                                                      nodes=self._f2u_nodes,
                                                      elements=self._f2u_elements)
            self._number.append(load_number)

    def __getitem__(self, load_name: str|int):
        """
        """
        try:
            return self._basic[load_name]
        except KeyError:
            raise IOError("Basic load case {:} not defined".format(load_name))

    #
    def __delitem__(self, load_name: str|int):
        """
        """
        del self._basic[load_name]
    #
    #
    # def get_basic_load(self, elements, nodes, materials,
    #                   sections):
    #    """
    #    """
    #    1/0
    #    #return get_basic_load(self, elements, nodes, 
    #    #                      materials, sections)
    #
#
#
#
class LoadTypeInMemory(LoadTypeBasic):
    """
    """
    __slots__ = ['_node', '_beam', '_selfweight',
                 'name', 'number', 'title', '_wave']
                 #'_f2u_beams', '_f2u_nodes']

    def __init__(self, name: str | int, number: int, title: str,
                 nodes, elements):
        """
        """
        super().__init__(name, number, title)
        #self.name = name
        #self.number = number
        #self.title = title
        self._selfweight = SelfWeight()
        self._node = NodeLoadItemIM(load_name=name,
                                    load_title = title, 
                                    nodes=nodes)
        beams = elements.beams()
        self._beam = BeamLoadItemIM(load_name=name,
                                    load_title = title,
                                    beams=beams)
        #
        self._wave = WaveLoadItemIM(load_name=name)
    #
#
