# 
# Copyright (c) 2009-2019 fem2ufo
#

# Python stdlib imports
from array import array
from collections.abc import Mapping
#from dataclasses import dataclass
from typing import NamedTuple, Tuple, List, Iterator, Dict, Iterable, Union


# package imports
#from steelpy.f2uModel.properties.beam import HydroBeamProperties


#
#
class BeamConcept(Mapping):
    """
    FE Element Load Class
    
    ElementLoad
        |_ name
        |_ number
    
    **Parameters**:  
      :number:  integer internal number 
      :name:  string node external name
    """
    __slots__ = ['_labels', '_number',
                 '_beams', '_hydro', '_properties']
    
    def __init__(self, properties, beams) -> None:
        """
        """
        self._beams = beams
        self._properties = properties
        self._labels : List[Union[int, str]] = []
        self._number : List[int] = array('I', [])
        #self._hydro = HydroBeamProperties(properties)
    #
    #
    def __setitem__(self, concept_name: int, property_type: str) -> None:
        """
        farg = [name, connectivity, material, section, type, group]
        """
        try:
            _element = self._beams[concept_name]
            self._hydro[concept_name] = property_type
        except Exception as e:
            raise KeyError(e)

    def __getitem__(self, element_name: Union[int, str]) -> Tuple:
        """
        """
        try:
            self._index = self._labels.index(element_name)
        except ValueError:
            _number = len(self._labels) + 1
            self.__setitem__(element_name, _number)
        return BeamProperties(self, element_name)

    #
    #
    def __iter__(self):
        """
        """
        return iter(self._labels)

    def __contains__(self, value) -> bool:
        return value in self._labels

    def __len__(self) -> float:
        return len(self._labels)

    #def __delitem__(self, element_number: int) -> None:
    #    """
    #    """
    #    del self._distributed[element_number]
    #    del self._mass[element_number]
#
#
#
class BeamProperties:
    """
    """
    
    def __init__(self, cls, element_name):
        """
        """
        #return
        self._cls = cls
        self._element_name = element_name
        #self._hydro = HydroBeamProperties(properties)
    #
    #
    def __getattr__(self, value):
        """
        """
        if value.lower() in ['cdcm']:
            _item_number = self._cls._cdcm[self._element_name]
            _element_number = self._cls._properties.hydrodynamic.CdCm._cdcm.get_item_name(_item_number)
            return self._cls._properties.hydrodynamic.CdCm[_element_number]
        
        elif value.lower() in ['flooded']:
            _item_number = self._cls._flooded[self._element_name]
            _element_number = self._cls._properties.hydrodynamic._flooded.get_item_name(_item_number)
            return self._cls._flooded[_element_number]
        
        elif value.lower() in ['marine_growth']:
            _item_number = self._cls._marine_growth[self._element_name]
            _element_number = self._cls._properties.hydrodynamic._marine_growth.get_item_name(_item_number)            
            return self._cls._marine_growth[_element_number]
        
        elif value.lower() in ['diameter']:
            _item_number = self._cls._hydro_diameter[self._element_name]
            _element_number = self._cls._properties.hydrodynamic._hydro_diameter.get_item_name(_item_number)            
            return self._cls._hydro_diameter[_element_number]
        
        else:
            print('--> fix here <-- beam properties concept')

    def __setattr__(self, key, value):
        """
        """
        if key.lower() in ['cdcm']:
            _item = self._properties.metocean.CdCm[value]
            #_item.sets.elements.append(self.element_number)
            self._cls._hydro[self._element_name]._cdcm = _item.number
            return
            
        elif key.lower() in ['flooded']:
            if value:
                self._cls._hydro[self._element_name]._flooded= 1
            else:
                self._cls._hydro[self._element_name]._flooded = 0
            return
            
        elif key.lower() in ['marine_growth']:
            _item = self._properties.hydrodynamic.marine_growth[value]
            _item.sets.elements.append(self.element_number)
            self._cls._hydro[self._element_name]._marine_growth = _item.number
            return
            
        elif key.lower() in ['diameter']:
            _item = self._properties.hydrodynamic.hydro_diameter[value]
            _item.sets.elements.append(self.element_number)
            self._cls._hydro[self._element_name]._hydro_diameter = _item.number
            return
        
        else:
            super().__setattr__(key, value)
#
