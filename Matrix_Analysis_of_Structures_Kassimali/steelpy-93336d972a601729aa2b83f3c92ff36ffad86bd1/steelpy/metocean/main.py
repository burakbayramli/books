# 
# Copyright (c) 2019-2023 steelpy
#
from __future__ import annotations
# 
# Python stdlib imports
#from typing import Union, Dict
from collections.abc import Mapping
import re

# package imports
#from steelpy.metocean.irregular.main import WaveIrregular
from steelpy.metocean.regular.main import RegularWaves
#from steelpy.metocean.irregular.spectrum import Sprectrum
from steelpy.utils.units.main import Units
#from steelpy.metocean.interface.wave import RegularWaves, IregularWaves
#from steelpy.metocean.interface.wind import Winds
from steelpy.metocean.current.main import Current
from steelpy.metocean.hydrodynamic.marine_growth import MarineGrowth
from steelpy.metocean.hydrodynamic.morison import CdCmCoefficients
from steelpy.metocean.process.combination import MetoceanCombination
from steelpy.metocean.regular.process.bsotm import BSOTM
from steelpy.metocean.hydrodynamic.main import Hydrodynamic
#
#
class Metocean:
    """
    FE Metocean Class
    
    Metocean
        |_ name
        |_ number
        |_ data
        |_ type
        |_ wave
        |_ current
        |_ wind
        |_ cdcm
        |_ non_hydro
        |_ elevation
        |_ hydro_diameter
        |_ buoyancy
        |_ flooded
        |_ seastate
    
    **Parameters**:  
      :number:  integer internal number 
      :name:  string node external name
      
      : seastate : metocean combination
    """
    #
    __slots__ = ['_wave', '_current', '_wind',
                 '_cdcm','_combination', '_rho_w',
                 '_hydrodynamic', '_marine_growth']
    #
    def __init__(self):
        """
        """
        self._rho_w: float = 1032.0  # kg / m^
        self._wave = WaveType(rho_w=self._rho_w)
        #self._wind = Winds()
        self._current = Current()
        self._marine_growth = MarineGrowth(rho_w=self._rho_w)
        self._cdcm = CdCmCoefficients()
        #
        self._hydrodynamic = Hydrodynamic(rho_w=self._rho_w)
        self._combination = MetoceanCombination(wave=self._wave,
                                                current=self._current,
                                                marine_growth=self._marine_growth,
                                                cdcm=self._cdcm,
                                                hydro=self._hydrodynamic)
    #
    @property
    def rho_w(self):
        """ """
        units = Units()
        return self._rho_w * units.kg / units.m**3

    @rho_w.setter
    def rho_w(self, value:Units):
        """ """
        self._rho_w = value.convert('kilogram/metre^3').value
    #
    def wave(self, values:None|list=None,
             df=None):
        """ """
        if values:
            print('-->')
            1/0
        else:
            try:
                df.columns
                grpwave = df.groupby('type')
                for wtype, item in grpwave:
                    if re.match(r"\b(regular(_wave)?)\b", wtype, re.IGNORECASE):
                        self._wave._regular.df(df=item)
                    elif re.match(r"\b(iregular(_wave)?)\b", wtype, re.IGNORECASE):
                        # self._wave._iregular.df(df=item)
                        raise NotImplementedError
                    else:
                        raise ValueError("wave type invalid")
            except AttributeError:
                pass
        #
        return self._wave
    #
    ##
    #@property
    #def wind(self):
    #    """
    #    """
    #    return self._wind  
    #
    #@property
    def current(self, values:None|list=None,
                df=None):
        """
        """
        if values:
            print('-->')
            1/0
        else:
            try:
                df.columns            
                self._current.df(df)
            except AttributeError:
                pass
        return self._current
    #
    #@property
    def CdCm(self):
        """ """
        return self._cdcm
    #
    #
    def MG(self, values:None|list=None,
           df=None):
        """Marine Growth"""
        if values:
            print('-->')
            1/0
        else:
            try:
                df.columns            
                self._marine_growth.df(df)
            except AttributeError:
                pass           
        return self._marine_growth
    #
    #@property
    def hydro(self):
        """
        """
        return self._hydrodynamic
    #
    #@property
    def load(self, values:None|list=None,
             df=None):
        """
        """
        if values:
            print('-->')
            1/0
        else:
            try:
                df.columns            
                self._combination.df(df)
            except AttributeError:
                pass        
        return self._combination
    #
    #
    #
    def get_load(self, mesh, kinematic,
                 condition:int|None = None,
                 rho:float = 1025):
        """
        condition :
            1 - Linear wave (dafault)
            2 - Non-linear wave
        """
        #if not condition:
            
        wforce = BSOTM(kinematic, condition, rho)
        wforce.wave_force(mesh=mesh)
        print('-->')
    #
    #
    def pile_response(self, D:float|Units,
                      L:float|Units,
                      kinematic,
                      condition: int = 1,
                      rho:float = 1025):
        """
        D : Pile diametre
        L : Pile length
        kinematics : kinematic dataframe
        condition :
            1 - Linear wave (dafault)
            2 - Non-linear wave
        """
        Dp = D.convert('metre').value
        Lp = L.convert('metre').value
        #
        #if kinematic._type == 'regular':
        #bs, ovtm = bsotm_reg(kinematic, D_pile, condition)
        #else:
        #    #bs, ovtm = bsvtm(kinematic, D_pile, condition)
        #    raise NotImplemented
        #
        bsotm = BSOTM(kinematic, condition, rho)
        bs, otm = bsotm.solveBSOTM(D=Dp, L=Lp)
        #
        return bs, otm 
        #return surface
#
#
class WaveType(Mapping):
    __slots__ = ['_regular', '_iregular', '_spectrum',
                 '_rho_w', '_labels','_type']
    def __init__(self, rho_w:float):
        """
        """
        self._labels: list[str|int] = []
        self._type: list = []
        self._rho_w: float = rho_w  # kg / m^3
        self._regular = RegularWaves(cls=self)
        #self._iregular_wave = WaveIrregular()
        # self._spectrum = Sprectrum()
    #
    #
    def __setitem__(self, name: int|str,
                    wave_data: list|tuple|dict) -> None:
        #try:
        #    self._labels.index(name)
        #    raise Exception(f'Wave {name} already exist')
        #except ValueError:
        #    wave_type = wave_data[0]
        #    #properties = get_sect_properties(properties[1:])
        #    self._labels.append(name)
        #    self._type.append(wave_type)
        wave_type = wave_data[0]
        self._input(name, wave_type)
        #
        if re.match(r"\b(regular(_wave)?)\b", wave_type, re.IGNORECASE):
            self._regular[name] = wave_data
        elif re.match(r"\b(iregular(_wave)?)\b", wave_type, re.IGNORECASE):
            #self._iregular[name] = wave_data
            raise NotImplementedError
        else:
            raise ValueError("wave type invalid")

    def __getitem__(self, name: int|str):
        """
        node_name : node number
        """
        try:
            index = self._labels.index(name)
            wave_type = self._type[index]
        except ValueError:
            raise KeyError(f'   *** Wave {name} does not exist')
        #
        if re.match(r"\b(regular(_wave)?)\b", wave_type, re.IGNORECASE):
            return self._regular[name]
        elif re.match(r"\b(iregular(_wave)?)\b", wave_type, re.IGNORECASE):
            # return self._iregular[name]
            raise NotImplementedError
        else:
            raise ValueError("wave type invalid")
    #
    def _input(self, name:inst|str, wave_data:str):
        """ """
        try:
            self._labels.index(name)
            raise Exception(f'Wave {name} already exist')
        except ValueError:
            wave_type = wave_data
            self._labels.append(name)
            self._type.append(wave_type)
    #
    def __len__(self):
        return len(self._labels)

    def __iter__(self):
        return iter(self._labels)

    def __contains__(self, value):
        return value in self._labels
    #
    #
    #@property
    #def spectrum(self):
    #    """
    #    """
    #    return self._spectrum
    #
    #@property
    #def irregular(self):
    #    """
    #    """
    #    return self._iregular_wave
    #
    #@property
    def regular(self, values:None|list=None,
                df=None):
        """
        """
        if values:
            print('-->')
            1/0
        else:
            try:
                df.columns
                self._regular.df(df)
            except AttributeError:
                pass
        return self._regular
    #
#
#

