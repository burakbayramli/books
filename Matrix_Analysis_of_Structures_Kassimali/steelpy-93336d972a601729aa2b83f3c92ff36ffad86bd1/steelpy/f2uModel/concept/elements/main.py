# 
# Copyright (c) 2009 steelpy
#
#
# Python stdlib imports
from __future__ import annotations
#import math 
from itertools import chain
from array import array
#from collections import OrderedDict
from collections import Counter
from collections.abc import Mapping
import functools
import re
#import logging
#from typing import Dict, List, ClassVar, Tuple, Iterable, Union


# package imports
#
#from steelpy.beam.main import Beam
#
# steelpy.f2uModel.mesh.inmemory
#from .beam_ import BeamIM
from .beam import ConceptBeam
#
#
#
#
@functools.lru_cache(maxsize=2048)
def get_node_end(_number_nodes: int, line: int):
    """
    """
    if _number_nodes == 3:
        # print('triangle')
        if line == 1:
            return 1, 2
        elif line == 2:
            return 0, 2
        else:
            return 0, 1
    else:
        # print('quad')
        if line == 1:
            return 0, 1
        elif line == 2:
            return 1, 2
        elif line == 3:
            return 2, 3
        else:
            return 3, 0
#
#
#
class ConceptElements(Mapping):
    __slots__ = ['_type', '_labels',
                 '_beams', '_shells', 
                 '_materials', '_sections'] 

    
    def __init__(self, points, materials, sections) -> None:
        """
        Manages f2u elements
        """
        #
        # f2u classes
        #self._f2u_nodes = nodes
        self._materials = materials
        self._sections = sections
        #
        self._labels:list = []
        self._type:list = []
        #self._number: array = array('i', [])
        #
        #self._beams = BeamIM(nodes=nodes, materials=materials, sections=sections)
        self._beams = ConceptBeam(beam_type="beam",
                                  points=points,
                                  materials=materials,
                                  sections=sections,
                                  labels=self._labels,
                                  element_type=self._type)
    #
    #
    #@property
    #def _labels(self):
    #    """ """
    #    labels = self._beams._labels
    #    return labels
    #
    def __setitem__(self, element_name: int, parameters: list) -> None:
        """
        parameters = ['beam', node1, node2, material, section, roll_angle]
        """
        try:
            self._labels.index(element_name)
            raise Exception('element {:} already exist'.format(element_name))
        except ValueError:
            element_type = parameters[0]
            #mnumber = next(self.get_number())
            # default
            #self._labels.append(element_name)
            #self._number.append(mnumber)
            self._type.append(element_type)
            #
            if re.match(r"\b(shell(s)?|plate(s)?)\b", element_type, re.IGNORECASE):
                #return self._curve[mat_number]
                raise NotImplementedError('shell element tobe implemented')
            elif re.match(r"\b(beam)\b", element_type, re.IGNORECASE):
                self._beams[element_name] = parameters[1:]
            else:
                raise IOError(f' element type {element_type} not recognised')            
    
    def __getitem__(self, element_name: int|str):
        """
        """
        try:
            index = self._labels.index(element_name)
            element_type = self._type[index]
        except ValueError:
            raise KeyError('Invalid element name : {:}'.format(element_name))        
        #
        if re.match(r"\b(shell(s)?|plate(s)?)\b", element_type, re.IGNORECASE):
            raise NotImplementedError('shell element tobe implemented')
        elif re.match(r"\b(beam)\b", element_type, re.IGNORECASE):
            return self._beams[element_name]
        else:
            raise IOError(f' element type {element_type} not recognised') 

    #
    def __iter__(self):
        """
        """
        return iter(self._labels)

    def __delitem__(self, element_name: str|int) -> None:
        """
        """
        i = self._labels.index(element_name)
        self._labels.pop(i)
        #self._number.pop(i)
        self._type.pop(i)
        del self._beams[element_name]

    def __contains__(self, value) -> bool:
        return value in self._labels

    def __len__(self) -> float:
        return len(self._labels)
    #
    #
    #def get_number(self, start:int=0):
    #    """
    #    """
    #    try:
    #        n = max(self._labels)
    #    except ValueError:
    #        n = start
    #    #
    #    while True:
    #        n += 1
    #        yield n
    #
    #def iter_elements(self, arraysize=1000):
    #    """
    #    """
    #    for element_name in self._labels:
    #        yield BeamElement(self, element_name)
    #
    #def spcl_bc(self, element_type:str):
    #    """
    #    Impose condition for special global shapes
    #    1 fix (0 free)
    #    """
    #    if element_type == 'truss':
    #        return [1,1,1,1,0,0]
    #    else: # beam
    #        return [1,1,1,1,1,1]
    #
    @property
    def get_free_nodes(self):
        """
        find nodes not sharing elements
        """
        # FIXME : beam element only
        #columns = list(zip(*self._connectivity))
        #column = columns[0]
        #column
        #
        #flat = list(chain.from_iterable(self._connectivity))
        # Beam
        flat = list(chain.from_iterable(self._beams._connectivity))
        # Shell? 
        return [k for k, v in Counter(flat).items() if v == 1]
    #
    #
    #@property
    #def get_connectivities(self):
    #    """ """
    #    1/0
    #    return self._beams._connectivity
    #
    #
    #@property
    def beams(self, values:None|list=None,
              df=None):
        """
        """
        if values:
            if isinstance(values, list):
                for item in values:
                    element_name = item[0]
                    #try:
                    #    self._labels.index(element_name)
                    #    raise Exception('element {:} already exist'.format(element_name))
                    #except ValueError:
                        #element_type = 'beam'
                        #mnumber = next(self.get_number())
                        # default
                        #self._labels.append(element_name)
                        #self._number.append(mnumber)
                        #self._type.append(element_type)
                    self._beams[element_name] = item[1:]
        #
        # dataframe input
        try:
            df.columns
            self._beams.df = df
        except AttributeError:
            pass
        #
        return self._beams
        #return ElementType(item_type='beam',
        #                   cls_type=self._beams, cls=self)
#
#
class ElementType(Mapping):
    __slots__ =  ['_type', '_labels', '_number',
                  '_cls_type', '_item_type']
    
    def __init__(self, item_type: str, cls_type, cls):
        """
        """
        self._cls_type = cls_type
        self._item_type = item_type
        self._labels = cls._labels
        self._type = cls._type
        self._number = cls._number
    #
    def __setitem__(self, item_name:str|int,
                    properties:list[str|float]) -> None:
        """
        item_name : element number
        properties : [material, section, node1, node2, roll_angle]
        """
        self._labels.append(item_name)
        self._type.append(self._item_type)
        mat_number = next(self.get_number())
        self._number.append(mat_number)
        self._cls_type[item_name] = properties
    
    def __getitem__(self, material_name:str|int):
        """
        """
        #index = self._labels.index(material_name)
        #mat_number = self._number[index]        
        return self._cls_type[material_name]
    #
    #
    def __len__(self) -> float:
        return len(self._cls_type._labels)

    def __iter__(self):
        """
        """
        return iter(self._cls_type._labels)

    def __contains__(self, value) -> bool:
        return value in self._cls_type._labels    
    #
    #
    def __str__(self) -> str:
        """ """
        #print('--')
        return self._cls_type.__str__()
    #    
    #
    @property
    def df(self):
        """ """
        return self._cls_type.df
    #
    def get_number(self, start:int=1):
        """
        """
        try:
            n = max(self._number) + 1
        except ValueError:
            n = start
        #
        while True:
            yield n
            n += 1
    #
    #
    #@property
    def get_connectivities(self):
        """ """
        #1/0
        return self._cls_type._connectivity
#
#
#
