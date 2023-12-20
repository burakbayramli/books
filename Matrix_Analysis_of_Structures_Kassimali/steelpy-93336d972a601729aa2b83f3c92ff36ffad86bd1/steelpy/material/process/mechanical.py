# 
# Copyright (c) 2009-2023 steelpy
# 
from __future__ import annotations
#
# Python stdlib imports
from array import array
from collections.abc import Mapping
from dataclasses import dataclass
import re
from typing import NamedTuple,  List, Union, Iterable

# package imports
from steelpy.utils.units.main import Units
from ..process.print_report import print_isomat
#from ..process.operations import get_isomat_prop_df
#
from steelpy.utils.dataframe.main import DBframework

#
#
@dataclass
class MaterialItem:
    """ [name, Fy, Fu, E, G, Poisson, density, alpha, title(optional)]
    """
    name: str|int
    number: int
    #grade: float|None
    Fy: float
    Fu: float|None
    E: float
    G: float
    poisson: float
    density: float
    alpha: float
    title: str|None = None
    type: str = "isotropic"
    #
    def __str__(self) -> str:
        """
        :return:
        """
        #return print_isomat(self)
        return ("{:1.4E} {:1.4E} {:1.4E} {:1.4E} {:1.4E} {:1.4E}\n"
                  .format(self.Fy, self.Fu, self.E, self.poisson,
                          self.density, self.G))
#
#
class MaterialIsotropic:
    __slots__ = ['_Fy', '_Fu', '_E', '_G', 'poisson',
                 '_density', '_alpha',
                 'title', 'type', 'label', 'units']

    def __init__(self, label:str | int, Fy:float, Fu: float | None,
                 E: float, G: float, poisson: float, density : float,
                 alpha: float):
        """ [name, Fy, Fu, E, G, Poisson, density, alpha, title(optional)]
        """
        self.label = label
        self._Fy = Fy
        self._Fu = Fu
        self._E = E
        self._G = G
        self.poisson = poisson
        self._density = density
        self._alpha = alpha
        self.title: str | None = None
        self.type: str = "isotropic"
        self.units = Units()
    #
    @property
    def Fy(self):
        return self._Fy * self.units.Pa
    
    
    @Fy.setter
    def Fy(self, item):
        self._Fy = item.convert("pascal").value
    
    #
    @property
    def E(self):
        return self._E * self.units.Pa
    
    @E.setter
    def E(self, item):
        self._E = item.convert("pascal").value
    
    #
    @property
    def G(self):
        return self._G * self.units.Pa
    
    @G.setter
    def G(self, item):
        self._G = item.convert("pascal").value
        #
    
    @property
    def Fu(self):
        try:
            1 / self._Fu
            return self._Fu * self.units.Pa
        except ZeroDivisionError:
            return self._Fy / 0.75 * self.units.Pa
    
    @Fu.setter
    def Fu(self, item):
        """
        Fu :
        """
        self._Fu = item.convert("pascal").value
    
    #
    @property
    def density(self):
        return self._density * self.units.kg / self.units.m ** 3
    
    @density.setter
    def density(self, item):
        self._density = item.value
    
    #
    #
    @property
    def alpha(self):
        """"""
        return self._alpha * self.units.K
    
    #
    @property
    def name(self):
        """ """
        return self.title
    
    @name.setter
    def name(self, name:Union[str, int]):
        """ """
        self._title = name
    #
    #def __getattr__(self, attr):
    #    """
    #    Getter for myattr
    #    :param attr:
    #    :return:
    #    """
    #    #if attr in self.__slots__:
    #    #    return self[attr]
    #
    #    if re.search(r"\bFy\b", attr, re.IGNORECASE):
    #        return self._Fy * self.units.Pa
    #
    #    elif re.search(r"\bE\b", attr, re.IGNORECASE):
    #        return self._E * self.units.Pa
    #
    #    elif re.search(r"\bG\b", attr, re.IGNORECASE):
    #        return self._G * self.units.Pa
    #
    #    elif re.search(r"\bFu\b", attr, re.IGNORECASE):
    #        try:
    #            1 / self._Fu
    #            return self._Fu * self.units.Pa
    #        except ZeroDivisionError:
    #            return self._Fy / 0.75 * self.units.Pa
    #
    #    elif re.search(r"\bdensity\b", attr, re.IGNORECASE):
    #        return self._density * self.units.kg / self.units.m ** 3
    #
    #    elif re.search(r"\balpha\b", attr, re.IGNORECASE):
    #        return self._alpha * self.units.K
    #
    #    #elif re.search(r"\bpoisson\b", attr, re.IGNORECASE):
    #    #    return self.cls._poisson[self.index]
    #
    #    elif re.search(r"\bname\b", attr, re.IGNORECASE):
    #        return self.title
    #
    #    #elif re.search(r"\bname\b", attr, re.IGNORECASE):
    #    #    return self.cls._title[self.index]
    #    #
    #    #elif re.search(r"\bt(w)?\b", attr, re.IGNORECASE):
    #    #    return self.thickness
    #
    #    else:
    #        try:
    #            return self.__dict__[attr]
    #            #return super().__getattr__(self, attr)
    #        except KeyError:
    #            raise AttributeError(f"Variable {attr} not found")
    ##
    #def __setattr__(self, attr, value):
    #    """
    #    Setter for myattr
    #    :param attr:
    #    :return:
    #    """
    #    if re.search(r"\bFy\b", attr, re.IGNORECASE):
    #        self._Fy = value.convert("pascal").value
    #
    #    elif re.search(r"\bE\b", attr, re.IGNORECASE):
    #        self._E = value.convert("pascal").value
    #
    #    elif re.search(r"\bG\b", attr, re.IGNORECASE):
    #        self._G = value.convert("pascal").value
    #
    #    elif re.search(r"\bFu\b", attr, re.IGNORECASE):
    #        self._Fu = value.convert("pascal").value
    #
    #    elif re.search(r"\bdensity\b", attr, re.IGNORECASE):
    #        self._density = value.convert("kilogram/metre^3").value
    #
    #    elif re.search(r"\balpha\b", attr, re.IGNORECASE):
    #        self._alpha = value.convert("kelvin").value
    #
    #    #elif re.search(r"\bpoisson\b", attr, re.IGNORECASE):
    #    #    self._poisson = value
    #
    #    elif re.search(r"\bname\b", attr, re.IGNORECASE):
    #        self.title = value
    #
    #    #elif re.search(r"\bname\b", attr, re.IGNORECASE):
    #    #    return self.cls._title[self.index]
    #
    #    # elif re.search(r"\bt(w)?\b", attr, re.IGNORECASE):
    #    #    return self.thickness
    #
    #    else:
    #        try:
    #            self.__dict__[attr] = value
    #            #super().__setattr__(attr, value)
    #        except KeyError:
    #            raise AttributeError(f"Variable {attr} not found")
    #
    def __str__(self) -> str:
        """
        :return:
        """
        return print_isomat(self)

#
#
class GetMaterial:
    """ Linear Material Data"""

    __slots__ = ['number','index', 'cls', 'type', 'units']

    def __init__(self, cls, material_number:int) -> None:
        """
        """
        self.index = cls._labels.index(material_number)
        self.cls = cls
        self.number = material_number
        # get material name
        self.units = Units()
        self.type:str = "elastic"

    #
    #
    #
    #
    @property
    def Fy(self):
        return self.cls._Fy[self.index] * self.units.Pa
    
    
    @Fy.setter
    def Fy(self, item):
        self.cls._Fy[ self.index ] = item.convert("pascal").value
    
    #
    @property
    def E(self):
        return self.cls._E[ self.index ] * self.units.Pa
    
    @E.setter
    def E(self, item):
        self.cls._E[ self.index ] = item.convert("pascal").value
    
    #
    @property
    def G(self):
        return self.cls._G[ self.index ] * self.units.Pa
    
    @G.setter
    def G(self, item):
        self.cls._G[ self.index ] = item.convert("pascal").value
        #
    
    @property
    def Fu(self):
        try:
            1 / self.cls._Fu[ self.index ]
            return self.cls._Fu[ self.index ] * self.units.Pa
        except ZeroDivisionError:
            return self.cls._Fy[ self.index ] / 0.75 * self.units.Pa
    
    @Fu.setter
    def Fu(self, item):
        """
        Fu :
        """
        self.cls._Fu[ self.index ] = item.convert("pascal").value
    
    #
    @property
    def density(self):
        return self.cls._density[ self.index ] * self.units.kg / self.units.m ** 3
    
    @density.setter
    def density(self, item):
        self.cls._density[ self.index ] = item.value
    
    #
    #
    @property
    def alpha(self):
        """"""
        return self.cls._alpha[ self.index ] * self.units.K
    
    #
    @property
    def poisson(self):
        """"""
        return self.cls._poisson[ self.index ]
    
    
    @property
    def name(self):
        """ """
        return self.cls._title[self.index]
    
    @name.setter
    def name(self, name:Union[str, int]):
        """ """
        self.cls._title[self.index] = name
    #
    def get_name(self, number, _grade=False):
        """
        """
        if not _grade:
            try:
                _grade = round ( self.cls._grade[ self.index ] )
            except TypeError:
                _grade = 'Zero'
        # in case material already named (i.e. sacs case)
        _name = '_' + self.cls._title[ self.index ]
        if '_MT' in _name:
            _name = ''
        #
        self.cls._title[ self.index ] = 'MT' + str ( number ).zfill ( 3 ) + '_' + str ( _grade ) + _name
    #
    def equal(self, other, grade=None):
        """
        """
        if not grade:
            grade = other.grade
        # TODO : check if material type is needed
        if self.cls._type[ self.index ] == other.type \
                and self.cls._E[ self.index ] == other.E \
                and self.cls._grade[ self.index ] == grade \
                and self.cls._poisson[ self.index ] == other.poisson \
                and self.cls._density[ self.index ] == other.density:
            return True
        else:
            return False
    #
    def print_properties(self):
        """ """
        return print_isomat(self)
        # for line in text:
        #    print(line.rstrip())
    #
    #def set_default(self):
    #    """ """
    #    self.cls._cls._default = self.cls._title[self.index]
    #
    def __str__(self) -> str:
        return print_isomat(self)
#
class MaterialElastic(Mapping):
    """
    Represents an istotrop linear elastic material used for FEM simulations
    
    Material
        |_ name
        |_ number
        |_ type
        |_ Emodulus
        |_ poisson
        |_ Fy
        |_ density
        |_ alpha
        |_ damping
        |_ stiffness [k1, k2, k3,..., kn]
        |_ spring    [sp[0],...]
        |_ Gmodulus (shear modulus)
    
    **Parameters**:  
      :number:  integer internal number 
      :name:  string node external name
      :coordinate:  list node coordinates 
    """
    __slots__ = ['_E', '_poisson', '_density', '_default',
                 '_Fy', '_Fu', '_damping', '_alpha', #'f2u_units',
                  '_grade', '_G', '_labels', '_number', '_title']
    
    def __init__(self)-> None:
        """
        """
        #
        self._title: list[str|int] = []
        self._labels: list[str|int] = []
        self._number: array = array('I', [])
        self._grade: list[str|float] = []
        self._Fy: array = array('f', [])
        self._Fu: array = array('f', [])
        self._poisson: array = array('f', [])
        self._density: array = array('f', [])
        self._E: array = array('f', [])
        self._G: array = array('f', [])
        self._alpha: array = array('f', [])
    #
    def __setitem__(self, material_name: int|str,
                    properties: list[str|float]) -> None:
        """
        """
        try:
            self._labels.index(material_name)
            raise Exception('    *** warning material {:} already exist'
                            .format(material_name))
        except ValueError:
            self._labels.append(material_name)
            material_number = next(self.get_number())
            self._number.append(material_number)
            self._grade.append(-1)
            #
            self._title.append(properties.pop(0))
            # set properties with default when value missing
            self._Fy.append(properties[0]) # Pa
            self._Fu.append(properties[1]) # Pa
            self._E.append(properties[2])  # Pa
            self._G.append(properties[3])  # Pa
            self._poisson.append(properties[4])
            self._density.append(properties[5]) # kg/m^3
            self._alpha.append(properties[6])   # K
            #
            #index = self._labels.index(material_number)
            #self._Fy[index] = properties[0]
    #
    def __getitem__(self, material_number: int) -> tuple:
        """
        """
        try:
            index = self._labels.index(material_number)
            #return GetMaterial(self, material_number)
            return MaterialItem(name=self._labels[index], number=self._number[index],
                                Fy=self._Fy[index], Fu=self._Fu[index],
                                E=self._E[index], G=self._G[index],
                                poisson=self._poisson[index], density=self._density[index],
                                alpha=self._alpha[index])
        except ValueError:
            raise IndexError('   *** material {:} does not exist'.format(material_number))        
    #
    def get_number(self, start:int=1)-> Iterable[int]:
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
    def __len__(self) -> float:
        return len(self._labels)

    def __iter__(self):
        """
        """
        return iter(self._labels)

    def __contains__(self, value) -> bool:
        return value in self._labels
    #
    @property
    def df(self):
        """ raw data for dataframe"""
        #from steelpy.process.dataframe.dframe import DataFrame
        df = DBframework()
        matype = ['elastic' for _ in self._labels]
        data = {"name": self._labels,
                "number":self._number,
                "type" : matype,
                "Fy":self._Fy, "Fu":self._Fy,
                "E":self._E , "G": self._G,
                "poisson": self._poisson, 
                "density":self._density,
                "alpha" : self._alpha,
                "title":self._title, }
        #print('-->')
        return df.DataFrame(data)
    
    @df.setter
    def df(self, df):
        """ """
        #group = df.groupby("type")
        #elastic = group.get_group("elastic")
        #elastic = get_isomat_prop_df(elastic)
        #elastic = elastic.drop_duplicates(['name'])
        #xx = elastic.Fy.tolist()
        #
        material_number = [next(self.get_number()) for _ in df.name]
        #
        self._labels.extend(df.name.tolist())
        self._number.extend(material_number)
        self._grade.extend([-1 for _ in df.name])
        # Fill values
        self._title.extend(df.name.tolist())
        self._Fy.extend(df.Fy.tolist())
        self._Fu.extend(df.Fu.tolist())
        self._E.extend(df.E.tolist())
        self._G.extend(df.G.tolist())
        self._poisson.extend(df.poisson.tolist())
        self._density.extend(df.density.tolist())
        self._alpha.extend(df.alpha.tolist())
        #print('--')
#
#
class Spring(NamedTuple):
    """
    """
    #number: int
    force: List
    displacement: List
#
@dataclass
class Curve:
    """
    """
    __slots__ = ['type', '_spring', 'cls']
    
    def __init__(self, cls):
        """
        """
        self.cls = cls
    #
    @property
    def spring(self):
        """
        """
        _force = [_spring[1] for _spring in self._spring]
        _disp = [_spring[0] for _spring in self._spring]
        
        return Spring(force=_force, 
                      displacement=_disp)
    
    @spring.setter
    def spring(self, data):
        """
        """
        #self._force.append(data[0])
        #self._displacement.append(data[1])
        self._spring = data
#