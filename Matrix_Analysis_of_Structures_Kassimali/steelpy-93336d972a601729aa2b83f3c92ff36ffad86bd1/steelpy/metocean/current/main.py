# 
# Copyright (c) 2009-2023 steelpy
#
from __future__ import annotations
# Python stdlib imports
from collections.abc import Mapping
#from array import array
from dataclasses import dataclass
from operator import itemgetter
from typing import NamedTuple
import re

# package imports
from steelpy.utils.units.main import Number
import numpy as np
#
#
#
@dataclass
class CurrentItem:
    __slots__ = ['name', 'tvelocity', 'bvelocity',
                 'cprofile', '_profile', '_depth', '_eta','zd']
    def __init__(self, name:int|str, vel_top: float, vel_bottom: float,
                 profile: str = 'exponential'):
        """ """
        self.name = name
        self.tvelocity = vel_top
        self.bvelocity = vel_bottom
        self.cprofile = profile
        self._profile: list = []
        self._depth: float = 0.0
    #
    @property
    def profile(self):
        """ """
        if not self._profile:
            Vct = self.tvelocity
            #zd = (self.zd + self._depth) / max(self._depth + self._eta)
            zd = self.zd[::-1]
            val = (zd + self._depth) / max(self._depth + self._eta)
            #1/0
            val = np.power(np.abs(val), 1.0 / 7.0)
            profile = []
            for idx, item in enumerate(zd):
                profile.append([item, val[idx]])
            #self._profile = [self.zd, np.power(np.abs(Vct * zd), 1.0 / 7.0)]
            self._profile = profile
            #1/0
        #print('-->')
        return self._profile

    @profile.setter
    def profile(self, value: list[list]):
        """ """
        prof = []
        for item in value:
            #try:
            elev = item[0].value
            #except AttributeError:
            #    elev = item[0]
            factor = item[1]
            prof.append([elev, factor])
        #
        prof.sort(key=itemgetter(0), reverse=True)
        self._profile = prof
        #print('-->')
    #
    def seastate(self, d:float, z:list, eta:list):
        """ """
        try:
            1/ self._depth
        except ZeroDivisionError:
            self._depth = d
        #
        self._eta = eta
        self.zd = z
        #print('-->')
    #
    def Vc2(self, Vct: float, d: float, Z):
        """Current Velocity"""
        vc =  np.zeros((Z.shape))
        vc[:, :, :] = Vct
        vcr = np.power(np.abs(Vct * (Z + d), 1.0/7.0))
        vidx = Z < 0
        vc[vidx] = vcr[vidx]
        return vc
    #
    def Vc3(self, d: float, z: list, eta: list):
        """Current Velocity"""
        Vct = self.tvelocity
        zd = (z + d) / d
        vc = np.zeros((eta.size, zd.size))
        vc[:, :] = Vct
        #vcr1 = np.abs(Vct *  d)
        #
        ze = np.zeros((eta.size, zd.size))
        ze[:, :] = z
        zebool = np.transpose(ze[:, :].T < eta)
        #
        vcr = np.zeros((eta.size, zd.size))
        vcr[:, :] = np.power(np.abs(Vct * zd), 1.0/7.0)
        #
        vc[zebool] = vcr[zebool]
        return vc
    #
    def Vc(self, d: float, eta: list, zdepth: list):
        """Current Velocity"""
        Vct = self.tvelocity
        elev = list(reversed([item[0] for item in self.profile]))
        value = list(reversed([item[1] * Vct for item in self.profile]))
        #
        vc = np.zeros((eta.size, zdepth.size))
        #vc[:, :] = Vct
        for i, item in enumerate(eta):
            for j, point in enumerate(zdepth):
                vc[i,j] = np.interp(point, elev, value)
                                    #right=self.tvelocity)
        return vc
    #
#
#
#
class Current(Mapping):
    """
    """
    __slots__ = ['_current']
    
    def __init__(self):
        """
        """    
        self._current:dict = {}
        
    def __getitem__(self, name):
        """
        """
        return self._current[name]
    
    def __setitem__(self, name:str|int, value) -> None:
        """
        current_profile:str = linear/exponential/(constant, uniform, mean)
        """
        #1 / 0
        if isinstance(value, list|tuple|dict):
            data = self.get_data(value)
            #self._current[name] = CurrentItem(name=name,
            #                                  vel_top=data[0], vel_bottom=data[1],
            #                                  profile=data[2])
            #print('-->')
        elif isinstance(value, Number):
            data = self.get_data([value])
            #self._current[name] = CurrentItem(name=name,
            #                                  vel_top=data[0], vel_bottom=data[0],
            #                                  profile='uniform')
            #print('-->')

        else:
            raise IOError('data not valid')
        #
        self._current[name] = CurrentItem(name=name,
                                          vel_top=data[0],
                                          vel_bottom=data[1],
                                          profile=data[2])
    #
    def get_data(self, values):
        """ [velocity_top, velocity_bottom, profile (linear/exponential)] """
        outval = [0, 0, 'exponential', None]
        cprofile = outval[2]
        if isinstance(values, dict):
            1/0
        else:
            if isinstance(values[-1], str):
                cprofile = values.pop()
            #
            for idx in range(2):
                try:
                    item = values[idx]
                    #try:
                    outval[idx] = item.value
                    #except AttributeError:
                    #    outval[idx] = item
                except IndexError:
                    continue
        #
        if re.match(r"\b(constant|uniform|mean)\b", cprofile, re.IGNORECASE):
            outval[2] = 'uniform'
        #else:
        #    outval[2] = 'exponential'

        return outval
    #
    def __len__(self) -> int:
        return len(self._current)

    def __iter__(self):
        """
        """
        return iter(self._current)
    
    def __contains__(self, value) -> bool:
        return value in self._current
    #
    #
    def df(self, df, columns:dict|None=None):
        """ """
        grpname = df.groupby(['name'])
        for key, item in grpname:
            velmax = item["velocity"].max()
            self.__setitem__(name=key[0], value=velmax)
            #item["velocity"] /= value
            item["velocity"] =  [vel.value / velmax.value for vel in item["velocity"]]
            profile = item[["zlevel", "velocity"]].values.tolist()
            #zlevel =  item["zlevel"].values.tolist()
            self._current[key[0]].profile = profile
            
        #print('-->')
#
#
#
class MeanCurrent:
    __slots__ = ['c_type', '_current']
    
    def __init__(self):
        """ """
        self.c_type = 1
        self._current = 0
    #
    @property
    def Euler(self):
        """ Eularian mean """
        self.c_type = 1
         
    @Euler.setter
    def Euler(self, value:Units):
        """ Eularian mean """
        self._current = value.convert('metre/second').value
        self.c_type = 1     
    #
    @property
    def Stokes(self):
        """ Stokes depth integrated mean """
        self.c_type = 2
    
    @Stokes.setter
    def Stokes(self, value:Units):
        """ Stokes depth integrated mean """
        self._current = value.convert('metre/second').value
        self.c_type = 2