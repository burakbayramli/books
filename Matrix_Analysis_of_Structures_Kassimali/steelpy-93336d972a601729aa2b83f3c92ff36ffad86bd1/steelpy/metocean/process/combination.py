# 
# Copyright (c) 2019-2023 steelpy
#
# 
# Python stdlib imports
from __future__ import annotations

#from openpyxl.chart import title

#from array import array
from collections.abc import Mapping
#from collections import defaultdict
from dataclasses import dataclass
from typing import NamedTuple
#from math import prod

# package imports
from steelpy.utils.units.main import Units
# steelpy.f2uModel.load
from steelpy.metocean.process.bsotm import BSOTM
#import pandas as pd
#from steelpy.process.dataframe.main import DBframework
#from steelpy.f2uModel.load.process.combination import LoadCombinationBasic

#
#
#
class MetoceanCombination(Mapping):
    """
    FE Metocean Combination Class
    
    Combination
        |_ name
        |_ number
        |_ surface
        |_ gravity
        |_ water_density
        |_ air_density
        |_ fluid_viscosity
        |_ air_viscosity
        |_ zone
    
    **Parameters**:  
      :number:  integer internal number 
      :name:  string node external name
    """
    __slots__ = ['_combination', '_hydro', '_cdcm',
                 '_wave', '_current', '_marine_growth']

    def __init__(self, wave,
                 current, marine_growth,
                 cdcm, hydro):
        """
        """
        self._combination:dict = {}
        self._wave = wave
        self._current = current
        self._marine_growth = marine_growth
        self._hydro = hydro
        self._cdcm = cdcm
    #
    def __setitem__(self, comb_name: str|int, setupval: str|list|dict) -> None:
        """
        """
        #
        values = self.get_setup(setupval)
        comb_title = values.pop(0)
        self._combination[comb_name] = CombTypes(comb_name, comb_title, self)
        self._combination[comb_name].setup = values
    #
    def __getitem__(self, comb_name:str|int):
        """
        """
        return self._combination[comb_name]
    #
    def __delitem__(self, load_name:str|int):
        """
        """
        del self._combination[load_name]
    #
    def __len__(self) -> int:
        return len(self._combination)
    #
    def __iter__(self):
        """
        """
        return iter(self._combination)
    #
    #
    def get_setup(self, values:str|list|dict):
        """
        [title, marine_growth, CdCm, Buoyancy]
        """
        output = [None, False, False, False]

        if isinstance(values, str):
            output[0] = values

        elif isinstance(values, dict):
            raise NotImplementedError

        elif isinstance(values, list):
            for idx, item in enumerate(values):
                output[idx] = item

        else:
            raise IOError("input not valid")

        return output
    #
    #
    def df(self, df):
        """ """
        grpname = df.groupby(['name'])
        for key, item in grpname:
            name = key[0]
            comb = item[['title', 'MG', 'buoyancy',
                         'conductor_shielding']].values.tolist()
            wave = item[['wave_name', 'wave_direction',
                         'wave_kinematics', 'crest_elevation']].values.tolist()
            current = item[['current_name', 'current_direction',
                            'current_blockage', 'current_stretching']].values.tolist()
            wind = item[['wind_name', 'wind_direction']].values.tolist()
            #
            self.__setitem__(comb_name=name, setupval=comb[0])
            self._combination[name].wave = wave[0]
            self._combination[name].current = current[0]
            self._combination[name].wind = wind[0]
        #print('--')
        #1 / 0
    #
#
@dataclass
class Combination_Metocean:
    """
    """
    #__slots__ = []
    name:str|int
    #number:int
    wave:str|int = 0
    wave_direction:float = 0.0
    wave_kinematics:float = 0.0
    #
    buoyancy : bool = True
    #
    current: str|int = 0
    current_blockage:float  = 0.0
    current_stretching : bool = True
    current_direction:float = 0.0
    #
    wind:str|int = 0
    wind_direction:float = 0.0
    #wind_areas:list = []
#
#
class WaveBasic(NamedTuple):
    #number:int
    wave:str|int
    direction:float
    kinematics:float
    crest_elevation:float|bool
    title:str
#
class WindBasic(NamedTuple):
    #number:int
    wind:str|int
    direction:float
    #wind_areas:list = []
    title:str
#
class CurrentBasic(NamedTuple):
    #number:int
    current: str|int
    direction:float
    blockage:float
    stretching : bool
    title:str
#
class CombSetup(NamedTuple):
    """ """
    marine_growth: dict
    CdCm: dict
    hydro: dict
    buoyancy: str|int|bool
    #rho_w: float
#
#
class CombTypes:
    """
    """
    __slots__ = ['_wave', '_current', '_wind', '_setup',
                 'name', 'number', 'title', '_cls']
    
    def __init__(self, name:int|str, title:str, cls):
        """
        """
        #1/0
        self.name = name
        self.title = title
        #
        self._wave = None
        self._current = None
        self._wind = None
        self._cls = cls
    #
    @property
    def setup(self):
        """ comb setup"""
        return self._setup

    @setup.setter
    def setup(self, values=list):
        """ comb setup"""
        # marine_growth
        try:
            mg = self._cls._marine_growth[values[0]]
        except KeyError:
            units = Units()
            self._cls._marine_growth['default'] = [self._cls._hydro.rho_w * units.kg / units.m**3,
                                                   0 * units.mm, 'constant']
            mg = self._cls._marine_growth['default']
        # CdCm
        #try:
        CdCm = self._cls._cdcm
        #except KeyError:
        #    1/0
        #
        buoyancy = values[2]
        self._setup = CombSetup(marine_growth=mg,
                                CdCm=CdCm,
                                hydro=self._cls._hydro,
                                buoyancy=buoyancy)
    #
    @property
    def wave(self):
        """
        """
        return self._wave
    
    @wave.setter
    def wave(self, values: list|dict):
        """
        [wave_name, Direction(deg), Kinematics, title]
        """
        values, title = self.get_values(values)
        try:
            wave = self._cls._wave[values[0]]
        except KeyError:
            raise IOError(f'wave {values[0]} not found')
        self._wave = WaveBasic(wave=wave,
                               direction=values[1],
                               kinematics=values[2],
                               crest_elevation=values[3],
                               title=title)
    #
    @property
    def current(self):
        """
        """
        return self._current
    
    @current.setter
    def current(self, values: list|dict):
        """
        """
        values, title = self.get_values(values)
        try:
            current = self._cls._current[values[0]]
        except KeyError:
            raise IOError(f'current {values[0]} not found')
        self._current = CurrentBasic(current=current,
                                     direction=values[1],
                                     blockage=values[2],
                                     stretching=values[3],
                                     title=title)
    #
    @property
    def wind(self):
        """
        """
        return self._wind
    
    @wind.setter
    def wind(self, values: list|dict):
        """
        """
        values, title = self.get_values(values)
        self._wind = WindBasic(wind=values[0],
                               direction=values[1],
                               title=title)
    #
    # -----------------------------
    # load process
    #
    def load(self):
        """ convert to beam load"""
        wave = self._wave.wave
        current = self._current
        hydro = self._setup
        rho_w = self._cls._hydro.rho_w
        kinematics = wave.kinematics()
        res = BSOTM(kinematics=kinematics,
                    current=current,
                    hydro=hydro,
                    rho_w=rho_w, 
                    condition=2)
        return res
        #calc
    #
    # -----------------------------
    # operations
    #
    def get_values(self,values):
        """ """
        # TODO : values need update
        if isinstance(values, (list|tuple)):
            if isinstance(values[-1], str):
                title = values.pop(-1)
            else:
                title = ""

            try:
                values[3] = values[3].value
            except (AttributeError, IndexError):
                values.append(False)
            
        elif isinstance(values, dict):
            pass
        else:
            raise IOError('Input data not valid')
        #
        return values, title
    #
#