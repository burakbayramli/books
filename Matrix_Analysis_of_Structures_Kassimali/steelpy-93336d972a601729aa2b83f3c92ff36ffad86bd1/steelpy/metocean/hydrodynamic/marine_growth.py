# 
# Copyright (c) 2009-2023 fem2ufo
#
# Python stdlib imports
from __future__ import annotations
from collections.abc import Mapping
from dataclasses import dataclass
from operator import itemgetter
from typing import NamedTuple
import re

#
# package imports
from steelpy.utils.units.main import Number, Units
from steelpy.metocean.hydrodynamic.operations import BasicProperty, get_list
import numpy as np


class MarineGrowth(Mapping):
    """
    """
    __slots__ = ['_mg', '_rho_w'] #'f2u_units',
                 #'_rhow', '_default']
    
    def __init__(self, rho_w):
        """
        """
        #global f2u_units #, mg_default
        #f2u_units = Units()
        #
        #self._default: None|str = None
        self._mg:dict = {}
        self._rho_w:float = rho_w # 1032.0 * f2u_units.kg / f2u_units.m**3
    #
    def __getitem__(self, name: str|int):
        """
        """
        return self._mg[name]
    
    def __setitem__(self, name:str|int, value) -> None:
        """
        """
        #if re.match(r"\b(constant)\b", mg_type, re.IGNORECASE):
        #    self._mg[mg_name] = MGconstant()
        #elif re.match(r"\b(profile)\b", mg_type, re.IGNORECASE):
        #    self._mg[mg_name] = MGprofile(self)
        #else:
        #    raise IOError("marine growth type {:} not implemented".format(mg_type))
        #
        #self._mg[mg_name].name = mg_name
        #
        #
        if isinstance(value, list|tuple|dict):
            data = self.get_data(value)
        elif isinstance(value, Number):
            data = self.get_data([value])
        else:
            raise IOError('data not valid')
        #
        self._mg[name] = MGitem(name=name,
                                density=data[0],
                                thickness=data[1],
                                profile=data[2])
    #
    #
    def get_data(self, values):
        """ [density, thickness, constant/profile, tile] """
        outval = [0, 0, 'profile', None]
        mgprofile = outval[2]
        if isinstance(values, dict):
            1/0
        else:
            if isinstance(values[-1], str):
                mgprofile = values.pop()
            #
            outval[0] = values[0].convert('kilogram/metre^3').value
            try:
                outval[1] = values[1].value
                mgprofile = 'uniform'
            except IndexError:
                #outval[2] = 'profile'
                pass
        #
        if re.match(r"\b(constant|uniform|mean)\b", mgprofile, re.IGNORECASE):
            outval[2] = 'uniform'
        #
        return outval
    #
    #
    #
    #def add_new(self, mg_name, mg_number=None):
    #    """
    #    """
    #    if mg_number:
    #        _number = mg_number
    #    else:
    #        _number = len(self._mg) + self.number        
    #    self.__setitem__(mg_name, _number)
    #    
    #
    def __len__(self) -> int:
        return len(self._mg)

    def __iter__(self):
        """
        """
        return iter(self._mg)
    
    def __contains__(self, value) -> bool:
        return value in self._mg
    #
    #
    def df(self, df, columns:dict|None=None):
        """ """
        units = Units()
        grpname = df.groupby(['name'])
        for key, item in grpname:
            try:
                value = item["density"].max()
            except KeyError:
                value = self._rho_w * units.kg / units.m**3
            #
            self.__setitem__(name=key[0], value=value)
            profile = item[["zlevel", "thickness"]].values.tolist()
            self._mg[key[0]].profile = profile
        #print('-->')    
#
#
@dataclass
class MGitem:
    __slots__ = ['name', '_profile', 'mgprofile', 'thickness', 'density']
    #
    def __init__(self, name: int|str, density: float, thickness: float,
                 profile: str = 'uniform'):
        """ """
        self.name = name
        self.density = density
        self.thickness = thickness
        self.mgprofile = profile
        self._profile: list = []
        #self._depth: float = 0.0
    #
    @property
    def profile(self):
        """ """
        if not self._profile:
            1/0
            #Vct = self.tvelocity
            #zd = (self.zd + self._depth) / max(self._depth + self._eta)
            #zd = self.zd[::-1]
            #val = (zd + self._depth) / max(self._depth + self._eta)
            #1/0
            #val = np.power(np.abs(val), 1.0 / 7.0)
            #profile = []
            #for idx, item in enumerate(zd):
            #    profile.append([item, val[idx]])
            #self._profile = [self.zd, np.power(np.abs(Vct * zd), 1.0 / 7.0)]
            #self._profile = profile
            #1/0
        #print('-->')
        return self._profile

    @profile.setter
    def profile(self, value: list[list]):
        """ """
        prof = []
        for item in value:
            elev = item[0].value
            thickness = item[1].value
            prof.append([elev, thickness])
        #
        prof.sort(key=itemgetter(0), reverse=True)
        self._profile = prof
    #
    #
    def MGX(self, Z):
        """ MArine growth"""
        mgr = np.zeros((Z.shape))
        mgr[:, :, :] = 0.90 # m
        mgr[Z<-25.0] = 0.50 # m
        mgr[Z>2] = 0.0      # m
        return mgr
    #
    def MGXX(self, Z):
        """ MArine growth"""
        mgr = np.zeros((Z.shape))
        mgr[:] = 0.90 # m
        mgr[Z < -25.0] = 0.50 # m
        mgr[Z > 2] = 0.0      # m
        return mgr
    #
    def MG(self, Z):
        """ MArine growth"""
        prof = self.profile
        elev = list(reversed([item[0] for item in prof]))
        value = list(reversed([item[1] for item in prof]))
        mgr = np.zeros((Z.shape))
        for i, point in enumerate(Z):
            mgr[i] = np.interp(point, elev, value)
        #
        return mgr
    #    
    #    
#
#
#
#
# -------------------------------------------------------------
#
class Profile(NamedTuple):
    """
    """
    elevation : List
    thickness : List
#
class MGprofile(BasicProperty):
    """
    FE Marine Growth Class
    
    Marine_Growth
        |_ name
        |_ number
        |_ case : constant_thickness / depth_profile
        |_ profile
        |_ inertia
        |_ factor
        |_ items
        |_ sets
    
    **Parameters**:  
      :number:  integer internal number 
      :name:  string node external name
    """
    #
    __slots__ = ['option', '_units', 'case', #'name',
                 'inertia', 'factor', 'density', 
                 'density_factor', 'absolute_elevations',
                 '_elevation', '_thickness', '_roughness',
                 '_densities', '_label', '_cls']
    #
    def __init__(self, cls):
        """
        """
        BasicProperty.__init__(self)
        #
        self._cls = cls
        self.case = 'profile'
        #
        self.inertia : bool = True
        self.absolute_elevations : bool = False
        self.density_factor : float = 1.36
        self.density : float = 1400.0 #* f2u_units.kg / f2u_units.m**3
        #
        self._label : list[float] = []
        self._elevation : list[float] = []
        self._thickness: list[float] = []
        self._roughness: list[float] = []
        self._densities: list[float] = []
    #
    #
    #def __getitem__(self, level_name:Union[str,int]):
    #    """
    #    """
    #    print('--')
    #
    #def __setitem__(self, level_name:Union[str,int], data:List) -> None:
    @property
    def level(self):
        """
        level_name : Elevation ID
        data = [level, thickness, roughness, density]  
        ----------------------------------------------
        level : elevation above mudline
        thickness : marine growth thickness
        roughness : surface roughness (0m defaul)
        density : dry weight density of marine growth (1400kg/m3 defaul)
        """
        return MGtypes(cls=self)
    #
    #
    def get_density_factor(self, density):
        """
        """
        self.density_factor = density/self._wdensity
        return self.density_factor
    #
    def set_default(self):
        """
        """
        #print('--')
        self._cls._default = self.name
    #   
#
class MGconstant(BasicProperty):
    __slots__ = ['case', '_units', '_roughness',
                 'density', 'density_factor', '_thickness']
    
    def __init__(self):
        """
        """
        BasicProperty.__init__(self)
        self.case = 'constant_thickness'
        #
        self._roughness : float = 0.0
        self.density : float = 1400.0 #* f2u_units.kg / f2u_units.m**3
        self.density_factor : float = 1.36
    #
    #
    def constant(self, thickness, roughness, 
                 density_factor=None):
        """
        Marine Growth defined with a Constant thickness
        """
        self._thickness = thickness
        self._roughness = roughness
        if density_factor:
            self.density_factor = density_factor
    #
    @property
    def thickness(self):
        """
        """
        return self._thickness
    
    @thickness.setter
    def thickness(self, value):
        """
        """
        self._thickness = value
    #
    @property
    def roughness(self):
        """
        """
        return self._roughness
    
    @roughness.setter
    def roughness(self, value):
        """
        """
        self._roughness = value
#
#
class MGtypes:
    
    __slots__ = ["_cls"]
    
    def __init__(self, cls):
        """
        """
        self._cls = cls
    
    #
    #
    def __getitem__(self, level_name:str|int):
        """
        """
        print('--')
    #
    def __setitem__(self, level_name:str|int,
                    data:list) -> None:
        """
        """
        self._cls._label.append(level_name)
        #
        data = self._get_value(data, steps=4)
        self._cls._elevation.append(data[0])
        self._cls._thickness.append(data[1])
        self._cls._roughness.append(data[2])
        try:
            1/data[3]
            self._cls._densities.append(data[3])
        except ZeroDivisionError:
            self._cls._densities.append(1400.0)
        print('--')
    #
    #
    #
    #
    def _get_value(self, value, steps):
        """
        """
        if isinstance(value, (list, tuple)):
            value = get_list(value, steps)
        elif isinstance(value, dict):
            value = get_dic(value)
        else:
            raise Exception('   *** input format not recognized')
        return value
#
#
def get_dic(data)->list[float]:
    """ """
    new_data = [0,0,0,0]
    for key, item in data.items():
        if re.match(r"\b(elevation)\b", str(key), re.IGNORECASE):
            new_data[0] = item.value
        elif re.match(r"\b(thickness)\b", str(key), re.IGNORECASE):
            new_data[1] = item.value
        elif re.match(r"\b(roughness)\b", str(key), re.IGNORECASE):
            new_data[2] = item.value
        elif re.match(r"\b(density)\b", str(key), re.IGNORECASE):
            new_data[3] = item.value
    return new_data
#
#