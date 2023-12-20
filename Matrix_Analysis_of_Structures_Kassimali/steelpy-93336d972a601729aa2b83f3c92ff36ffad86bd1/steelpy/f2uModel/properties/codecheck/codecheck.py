# 
# Copyright (c) 2009-2019 fem2ufo
# 


# Python stdlib imports
from array import array
from collections.abc import Mapping
from dataclasses import dataclass
from typing import NamedTuple, Tuple, List, Iterator, ClassVar, Union, Dict
import re
# package imports



class Item(NamedTuple):
    """ Cartesian coordinate system"""
    #x: float
    y: float
    z: float
    number: int
    name:Union[str,int]
#
#
@dataclass
class Element:
    """
    """
    __slots__ = ['name', 'index', '_elements']

    def __init__(self, cls, element_name) -> None:
        """
        """
        self.index: int = cls._labels.index(element_name)
        self._elements: ClassVar = cls
        self.name: Union[int, str] = element_name
    
    
    #@property
    #def number(self) -> int:
    #    return self._elements._number[self.index]
    #
    #@number.setter
    #def number(self, value:int) -> None:
    #    """"""
    #    self._elements._number[ self.index ] = value    
    
    def __setattr__(self, att, value):
        """
        """
        if re.match(r"\b(Lb(\_)?(y|z)?)\b", att, re.IGNORECASE):
            if re.match(r"\b(Lb)\b", att, re.IGNORECASE):
                self._elements._Lby[self.index] = value[0]
                self._elements._Lbz[self.index] = value[1]
            elif re.match(r"\b(Lb(\_)?y)\b", att, re.IGNORECASE):
                self._elements._Lby[self.index] = value
            elif re.match(r"\b(Lb(\_)?z)\b", att, re.IGNORECASE):
                self._elements._Lbz[self.index] = value
            else:
                raise AttributeError            
        elif re.match(r"\b(L(\_)?(y|z)?)\b", att, re.IGNORECASE):
            if re.match(r"\b(L)\b", att, re.IGNORECASE):
                self._elements._Ly[self.index] = value[0]
                self._elements._Lz[self.index] = value[1]                
            elif re.match(r"\b(L(\_)?y)\b", att, re.IGNORECASE):
                self._elements._Ly[self.index] = value
            elif re.match(r"\b(L(\_)?z)\b", att, re.IGNORECASE):
                self._elements._Lz[self.index] = value
            else:
                raise AttributeError
        elif re.match(r"\b(Cm(\_)?(y|z)?)\b", att, re.IGNORECASE):
            if re.match(r"\b(Cm)\b", att, re.IGNORECASE):
                self._elements._Cmy[self.index] = value[0]
                self._elements._Cmz[self.index] = value[1]
            elif re.match(r"\b(Cm(\_)?y)\b", att, re.IGNORECASE):
                self._elements._Cmy[self.index] = value
            elif re.match(r"\b(Cm(\_)?z)\b", att, re.IGNORECASE):
                self._elements._Cmz[self.index] = value
            else:
                raise AttributeError            
        elif re.match(r"\b(K(\_)?(y|z)?)\b", att, re.IGNORECASE):
            if re.match(r"\b(K)\b", att, re.IGNORECASE):
                self._elements._Ky[self.index] = value[0]
                self._elements._Kz[self.index] = value[1]
            elif re.match(r"\b(K(\_)?y)\b", att, re.IGNORECASE):
                self._elements._Ky[self.index] = value
            elif re.match(r"\b(K(\_)?z)\b", att, re.IGNORECASE):
                self._elements._Kz[self.index] = value
            else:
                raise AttributeError
        else:
            super().__setattr__(att, value)
    
    def __getattr__(self, att):
        """
        """
        if re.match(r"\b(Lb(\_)?(y|z)?)\b", att, re.IGNORECASE):
            if re.match(r"\b(Lb)\b", att, re.IGNORECASE):
                return Item(self._elements._Lby[self.index],
                            self._elements._Lbz[self.index],
                            #self._elements._Lbz[self.index],
                            self._elements._number[self.index],
                            self._elements._labels[self.index])
            elif re.match(r"\b(Lb(\_)?y)\b", att, re.IGNORECASE):
                return self._elements._Lby[self.index]
            elif re.match(r"\b(Lb(\_)?z)\b", att, re.IGNORECASE):
                return self._elements._Lbz[self.index]
            else:
                raise AttributeError
        elif re.match(r"\b(L(\_)?(y|z)?)\b", att, re.IGNORECASE):
            if re.match(r"\b(L)\b", att, re.IGNORECASE):
                return Item(self._elements._Ly[self.index],
                            self._elements._Lz[self.index],
                            #self._elements._Lz[self.index],
                            self._elements._number[self.index],
                            self._elements._labels[self.index])
            elif re.match(r"\b(L(\_)?y)\b", att, re.IGNORECASE):
                return self._elements._Ly[self.index]
            elif re.match(r"\b(L(\_)?z)\b", att, re.IGNORECASE):
                return self._elements._Lz[self.index]
            else:
                raise AttributeError
        elif re.match(r"\b(Cm(\_)?(y|z)?)\b", att, re.IGNORECASE):
            if re.match(r"\b(Cm)\b", att, re.IGNORECASE):
                return Item(self._elements._Cmy[self.index],
                            self._elements._Cmz[self.index],
                            #self._elements._Cmz[self.index],
                            self._elements._number[self.index],
                            self._elements._labels[self.index])
            elif re.match(r"\b(Cm(\_)?y)\b", att, re.IGNORECASE):
                return self._elements._Cmy[self.index]
            elif re.match(r"\b(Cm(\_)?z)\b", att, re.IGNORECASE):
                return self._elements._Cmz[self.index]
            else:
                raise AttributeError
        elif re.match(r"\b(K(\_)?(y|z)?)\b", att, re.IGNORECASE):
            if re.match(r"\b(K)\b", att, re.IGNORECASE):
                return Item(self._elements._Ky[self.index],
                            self._elements._Kz[self.index],
                            #self._elements._Kz[self.index],
                            self._elements._number[self.index],
                            self._elements._labels[self.index])
            elif re.match(r"\b(K(\_)?y)\b", att, re.IGNORECASE):
                return self._elements._Ky[self.index]
            elif re.match(r"\b(K(\_)?z)\b", att, re.IGNORECASE):
                return self._elements._Kz[self.index]
            else:
                raise AttributeError
        else:
            raise IOError(' ** attribute {:} does not exist'.format(att))        
#
# CODE CHECK Section
class CodeCheck(Mapping):
    """
    FE Code Check Class
    
    CodeCheck
        |_ name
        |_ number
        |
        |_ Lb [x, y, z] (Unbraced length of the compression flange for local buckling due to bending)
    
    **Parameters**:  
      :number:  integer internal number 
      :name:  string node external name
    """
    __slots__ = ['_Lby','_Lbz',
                 '_Ly', '_Lz',
                 '_Kx', '_Ky', '_Kz',
                 '_Cmy', '_Cmz',
                 '_tapered', '_labels', '_number']

    def __init__(self):
        """
        """
        self._labels: List = []
        self._number : List[int] = array('I', [])        
        #
        #self.Lbx : List[float] = array('f', [])
        self._Lby : List[float] = array('f', [])
        self._Lbz : List[float] = array('f', [])
        #
        self._Kx : List[float] = array('f', [])
        self._Ky : List[float] = array('f', [])
        self._Kz : List[float] = array('f', [])
        #
        #self.Lx : List[float] = array('f', [])
        self._Ly : List[float] = array('f', [])
        self._Lz : List[float] = array('f', [])
        # Moment Modifiers
        #self.Cmx : List[float] = array('f', [])
        self._Cmy : List[float] = array('f', [])
        self._Cmz : List[float] = array('f', [])
    #
    def __setitem__(self, element_name:Union[str, int],
                    number: int) -> None:
        """
        """
        self._labels.append(element_name)
        self._number.append(number)
        #
        self._Lby.append(-1)
        self._Lbz.append(-1)
    #
    def __getitem__(self, element_name:Union[str, int]) -> Tuple:
        """
        node_number : node number
        """
        try:
            self._labels.index(element_name)
        except ValueError:
            number = len(self._labels) + 1
            self.__setitem__(element_name, number)
        return Element(self, element_name)
    #
    def __delitem__(self, element_name:Union[str, int]) -> None:
        """
        """
        try:
            i = self._labels.index(element_name)
            self._number.pop(i)
            self._labels.pop(i)
            #
            self._Lbx.pop(i)
            self._Lby.pop(i)
            self._Lbz.pop(i)
            #
            self._Kx.pop(i)
            self._Ky.pop(i)
            self._Kz.pop(i)
            #
            self._Lx.pop(i)
            self._Ly.pop(i)
            self._Lz.pop(i)
            #
            self._Cmx.pop(i)
            self._Cmy.pop(i)
            self._Cmz.pop(i)
        except IndexError:
            raise Warning(' delete -- element {:} does not exist'.format(element_name))
            return

    def __len__(self) -> float:
        return len(self._labels)

    def __iter__(self) -> Iterator:
        """
        """
        return iter(self._labels)

    def __contains__(self, value) -> bool:
        return value in self._labels    
#
#
#