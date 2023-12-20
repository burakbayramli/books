# 
# Copyright (c) 2009-2019 fem2ufo
# 


# Python stdlib imports
from array import array
from collections.abc import Mapping
from dataclasses import dataclass
#import logging
import re
from typing import NamedTuple, Tuple, List, Dict, Iterable, TypeVar, ClassVar, Union

# package imports
Tinp = TypeVar('Tinp', int, str)

#
#
#TODO: update group
#@dataclass
class Group:
    """
    """
    __slots__ = ['_name', '_number', '_f2u_concepts', '_elements', '_nodes',
                 '_concepts', '_mesh', '_loads', '_geometry']
    
    #
    def __init__(self, group_name:str, group_number:int,
                 concepts:ClassVar, mesh: ClassVar):
        """
        """
        self._name: str = group_name
        self._number: int = group_number
        self._concepts: List = []
        self._elements: List = []
        self._nodes: List = []
        self._f2u_concepts: ClassVar = concepts
        self._mesh: ClassVar = mesh
    #
    @property
    def name(self) -> str:
        """
        """
        return self._name
    @name.setter
    def name(self, group_name:str) -> None:
        """
        """
        self._name = group_name
    #
    @property
    def number(self) -> int:
        """
        """
        return self._number
    @number.setter
    def number(self, group_number:int) -> None:
        """
        """
        self._number = group_number
    #
    @property
    def nodes(self) -> List:
        """
        """
        _new_item: List[int] = []
        _delete: List[int] = []
        _nodes: ClassVar = self._mesh.nodes
        # 
        for _item in self._nodes:
            try:
                _elem = _nodes[_item]
                _new_item.append(_elem.number)
            except IndexError:
                _delete.append(_item)
        #
        for _item in _delete:
            self._nodes.remove(_item)
        return sorted(_new_item)
    
    @nodes.setter
    def nodes(self, values) -> None:
        """
        """
        if isinstance(values, (list, tuple)):
            for _item in values:
                self._mesh.nodes[_item]
                self._nodes.append(_item)
        elif isinstance(values, int):
            self._mesh.nodes[values]
            self._nodes.append(values)
        else:
            raise Exception('node data not valid {:}'.format(values))
    #
    @property
    def elements(self):
        """
        returns element number
        """
        _new_item = []
        _delete = []
        _elements = self._mesh.elements
        for _item in self._elements:
            try:
                _new_item.append(_elements[_item].number)
            except IndexError:
                _delete.append(_item)
        #_elements = [self._mesh.elements[_item].number
        #             for _item in self._elements]
        for _item in _delete:
            self._elements.remove(_item)
        #
        return sorted(_new_item)
    
    @elements.setter
    def elements(self, values):
        """
        """
        if isinstance(values, (list, tuple)):
            for _item in values:
                self._mesh.elements[_item]
                self._elements.append(_item)
        elif isinstance(values, int):
            self._mesh.elements[values]
            self._elements.append(values)
        else:
            raise Exception('element data not valid {:}'.format(values))
    #
    @property
    def concepts(self):
        """
        """
        return self._concepts
    
    @concepts.setter
    def concepts(self, values):
        """
        """
        if isinstance(values, (list, tuple)):
            for _item in values:
                _concept = self._f2u_concepts[_item]
                self._concepts.append(_item)
                # FIXME: why this?
                #self.elements = _concept._elements
        elif isinstance(values, str):
            _concept = self._f2u_concepts[values]
            self._concepts.append(values)
            # FIXME: why this?
            #self.elements = _concept._elements
            #print('-->')
        else:
            raise Exception('concept data not valid {:}'.format(values))
    #
    @property
    def geometry(self):
        """
        """
        print('-->')
#
#
class Groups(Mapping):
    """
    FE model groups
    
    Sets
        |_ name
        |_ number
        |_ type
        |_ items [m1, m2, m3,..., mn]
    
    **Parameters**:  
      :number:  integer internal number 
      :name:  string node external name
      :type:
      :items:
    """
    #
    __slots__ = ['_groups']

    def __init__(self) -> None:
        """
        """
        self._groups: Dict = {}
    #      
    #
    def __setitem__(self, set_name:Union[str, int], set_type:str) -> None:
        """
        """
        #self._groups[set_name] = Group(values, set_name)
        #try:
        #    self._groups[set_name]
        #    logging.warning(' ** set {:} already exists'.format(set_name))
        #except KeyError:
        #    self._groups[set_name] = Group(set_name, set_number,
        #                                   concepts=self._concepts,
        #                                   mesh= self._mesh)     
        #
        if re.match(r"\b(element(s)?)\b", set_type, re.IGNORECASE):
            self._groups[set_name] = Group()
        elif re.match(r"\b(node(s)?)\b", set_type, re.IGNORECASE):
            self._groups[set_name] = Group()
        elif re.match(r"\b(material(s)?)\b", set_type, re.IGNORECASE):
            self._groups[set_name] = Group()
        elif re.match(r"\b(_geometr(ies|y))\b", set_type, re.IGNORECASE):
            self._groups[set_name] = Group()        
        else:
            raise IOError("group type {:} not implemented".format(set_type))        
    #
    def __getitem__(self, set_name:str) -> Tuple:
        """
        """
        try:
            self._groups[set_name]
        except KeyError:
            raise KeyError
            #_number = len(self._groups)
            #self.__setitem__(set_name, _number)
        return self._groups[set_name]
    #
    def __iter__(self) -> Iterable:
        """
        """
        return iter(self._groups)
    
    def __len__(self) -> float:
        """
        """
        return len(self._groups)
    #
    def __delitem__(self, set_name:str) -> None:
        """
        """
        del self._groups[set_name]
    #
    @property
    def add_group(self) -> Dict:
        """
        """
        return None # self._groups
    
    @add_group.setter
    def add_group(self, set_name: str) -> None:
        """
        """
        try:
            self._groups[set_name]
            logging.warning(' ** set {:} already exists'.format(set_name))
        except KeyError:
            #FIXME: numbering missing
            set_number = len(self._groups) + 1
            self._groups[set_name] = Group(set_name, set_number,
                                           concepts=self._concepts,
                                           mesh=self._mesh)
            
    #
    def get_name(self, group_number: int = None):
        """
        """
        if not group_number:
            group_number = self.number
        #
        _name = ""
        if self.name != str(self.number):
            _name = '_'+ self.name
        #
        self.name = 'GR' + str(group_number).zfill(3) + _name
    #
    def clean(self):
        """
        """
        for _set in self._groups.values():
            #_new_item = []
            _elements = _set._mesh.elements
            _set.elements = self._select_items(_elements, _set.elements)
            #
            _nodes = _set._mesh.nodes
            _set.nodes = self._select_items(_nodes, _set.nodes)
            #
            _concepts = _set._f2u_concepts
            _set.concepts = self._select_items(_concepts, _set.concepts)
        #print('-->')
    #
    def _select_items(self, items, group):
        """ """
        _new_item = []
        for _name in group:
            try:
                items[_name]
                _new_item.append(_name)
            except IndexError:
                continue
        #group = _new_item
        return _new_item
