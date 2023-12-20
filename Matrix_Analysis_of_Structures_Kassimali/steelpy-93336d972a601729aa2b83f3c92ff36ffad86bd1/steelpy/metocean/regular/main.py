#
# Copyright (c) 2009-2023 steelpy
#
from __future__ import annotations
#
# Python stdlib imports
#from typing import NamedTuple
import re

# package imports
from steelpy.metocean.regular.process.waveops import get_wave_data
#from steelpy.metocean.regular.current.main_current import MeanCurrent
from steelpy.metocean.regular.stokes.Stokes import StokesModule
from steelpy.metocean.regular.fourier.Fourier import FourierModule
from steelpy.metocean.regular.cnoidal.Cnoidal import CnoidalModule

class RegularWaves:
    __slots__ = ['_wave', '_condition', '_cls',
                 '_stokes', '_fourier','_cnoidal']

    def __init__(self, cls)-> None:
        """
        """
        self._cls = cls
        self._condition = 2
        self._wave:dict = {}
        self._stokes = StokesModule()
        self._fourier = FourierModule()
        self._cnoidal = CnoidalModule()
    #
    #@property
    #def Stokes(self):
    #    """ """
    #    return StokesModule
    #
    #@property
    #def Cnoidal(self):
    #    """ """
    #    return CnoidalModule
    #
    #@property
    #def Fourier(self):
    #    """ """
    #    return FourierModule
    #
    #def current(self):
    #    """ """
    #    return self._current
    #
    def __setitem__(self, name: int|str,
                    case_data: list[float]|dict[str, float]) -> None:
        """
        case_name : Wave name
         H : Wave height [unit length]
         T : Wave Period [second]
         d : Water depth LTH + Tide and Surge [unit length]
         Lw : Wave Length [unit length]
         Phase : Wave phase [degree]
         Order : ??
        """
        #
        self._cls._input(name, 'regular')
        #
        data = get_wave_data(case_data)
        wave_type = data.pop()
        #
        if re.match(r"\b(stoke(s)?)\b", wave_type, re.IGNORECASE):
            self._wave[name] = 'stokes'
            self._stokes[name] = data
        
        elif re.match(r"\b(cnoidal)\b", wave_type, re.IGNORECASE):
            self._wave[name] = 'cnoidal'
            self._cnoidal[name] = data
        else:
            self._wave[name] = 'fourier'
            self._fourier[name] = data

        #self._wave[case_name] = case_data
    #
    #
    def __getitem__(self, name: int|str) -> tuple:
        """
        case_name : Wave name
        """
        if self._wave[name] == 'stokes':
            return self._stokes[name]
        
        elif self._wave[name] == 'cnoidal':
            return self._cnoidal[name]
        
        else:
            return self._fourier[name]
    #
    #
    def df(self, df):
        """ """
        grpname = df.groupby(['name'])
        for key, item in grpname:
            values = item[['Hw', 'Tw', 'd', 'theory']].values
            for idx, value in enumerate(values):
                # TODO : define input better
                #name = f'{key[0]}_{idx + 1}'
                name = key[0]
                self.__setitem__(name=name,
                                 case_data=value.tolist())
        #print('--')
    #
    #