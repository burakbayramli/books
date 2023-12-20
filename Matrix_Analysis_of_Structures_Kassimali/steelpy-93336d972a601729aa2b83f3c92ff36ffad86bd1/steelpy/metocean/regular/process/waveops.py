#
# Copyright (c) 2009-2023 steelpy
#
from __future__ import annotations
# Python stdlib imports
from array import array
from dataclasses import dataclass
import re
import math
#from typing import NamedTuple

# package imports
from steelpy.metocean.current.main import MeanCurrent
from steelpy.metocean.regular.process.kinematic import  get_kinematic, KinematicResults
from steelpy.metocean.regular.process.inout import get_Height
#
from steelpy.metocean.regular.process.surface import get_surface, SurfaceResults
#
#from steelpy.process.units.main import Units
#from steelpy.process.dataframe.main import DBframework
import numpy as np
#
#
#
#@dataclass
class WaveItem:
    __slots__ = ['H', 'Tw', 'd', 'title', 'Lw',
                 'order', 'nstep', 'niter', 'accuracy',
                 'current', 'c_type', 'method', 'finite_depth',
                 '_wave_length', '_z', '_Y', '_B', '_Tanh', '_Highest']
    
    def __init__(self, H:float, d:float, title:str, 
                 Tw:float|None=None,
                 Lw:float|None=None, 
                 infinite_depth:bool=False,
                 current:float = 0.0, c_type:int = 1,
                 order:int=5, nstep:int=2,
                 niter:int=40, accuracy:float=1e-6):
        """ """
        self.H = H
        if Lw:
            self.Lw = Lw
            self.Tw = None
        else:
            self.Tw = Tw
            self.Lw = None
        self.d = d
        self.title: str
        # current 
        self.current = current
        self.c_type = c_type        
        #
        self.order = order
        self.nstep = nstep
        self.niter = niter
        self.accuracy = accuracy
        #
        self.finite_depth = True
        if infinite_depth:
            self.finite_depth = False
        #
        #self.wave_length:float
        #
    #
    def get_parameters(self, g:float = 9.80665):
        """ 
        g: gravity # m/s^2
        
        returns:
        MaxH : H/d
        T : Dimensionless period / None
        L : Dimensionless wavelength / None
        c_type  : Current criterion, 1 for Euler, or 2 for Stokes
        current : Current magnitude
        order :  Number of Fourier components or order of Stokes or cnoidal theory
        nsteps : Number of height steps to reach H/d
        niter  : Maximum number of iterations for each step (20)
        crit   : Criterion for convergence (1e-6)
        Height : wave height
        finite_depth : True/False
        """
        MaxH = self.H / self.d
        current = self.current / math.sqrt( g * self.d )
        if self.Lw:
            case = "wavelength"
            L = self.Lw / self.d
            Height = get_Height(MaxH, case, self.finite_depth, L=L)
            T = None
        else:
            case = 'period'
            T = self.Tw * math.sqrt( g /  self.d )
            Height = get_Height(MaxH, case, self.finite_depth, T=T)
            L = None
        #
        return [MaxH, case, T, L, self.c_type, current,  
                self.order, self.nstep, self.niter, self.accuracy,
                Height, self.finite_depth]
    #
    @property
    def L(self):
        """ Wave Length"""
        try:
            return self._wave_length * self.d
        except AttributeError:
            self.__call__()
            return self._wave_length * self.d
    #
    #
    def surface(self, surface_points:int = 36,
                step_size:float|None=None):
        """Free surface (wave) profile
        surface_points : Number of points on free surface (the program clusters them near crest)
        step_size: deg
        """
        self.__call__()
        n = self.order
        kd = self._z[1]
        #
        surface = get_surface(n, kd, self._Y, self.d, 
                              surface_points, self.finite_depth)
        #
        return SurfaceResults(surface, self.H, self.Tw, self.d, self.finite_depth)
    #
    #
    def kinematics(self, surface=None, depth_points:int = 100,
                   surface_points:int = 36):
        """ """
        self.__call__()
        if not surface:
            surface = self.surface(surface_points)
        #
        #n = self.order
        #kd = self._z[1]
        #
        #crest = surface.eta
        #
        #depth = np.arange(depth_points + 1) / depth_points
        #
        #zdepth =  crestmax * depth  # --> fix sign
        #zdepth = surfacePoints(d=self.d, points=depth_points, eta=crest)
        #
        #get_kinematicX(n, self._z, self._Y, self._B, self._Tanh,
        #               surface_points, depth_points, self.finite_depth)
        #
        kpoints = depth_points
        kindf = get_kinematic(self.order, self._z, self._B, self._Tanh,
                              self.d, surface, kpoints,
                              self.finite_depth)
        #
        #
        return KinematicResults(surface, kindf, depth_points+1)
        #
        #return dataframe
#
#
#
def surfacePoints(d: float, points: int, eta: list,
                  stickup: float = 1.0):
    """get surface points"""
    crestmax = np.ceil(eta.max()) + stickup
    crestmin = np.floor(eta.min())    
    step1 = int(np.ceil(points / 2))
    step2 = int(points - step1)

    return np.hstack([np.linspace(-d, crestmin, step1, endpoint=False),
                      np.linspace(crestmin, 0, step2, endpoint=False),
                      np.linspace(0, crestmax, step2)])

#
#
#
class WaveRegModule:
    __slots__ = [ 'order', 'nsteps', 'max_iter', 'accuracy',
                  '_labels', '_cases', '_current', # 'c_type', 
                  'infinite_depth']

    def __init__(self, n:int=5, nstep:int=2,
                 number:int=40, accuracy:float=1e-6):
        """
        n      : Stokes order (5)
        nstep  : Number of height steps to reach H/d (2)
        number : Maximum number of iterations for each step (20)
        accuracy   : Criterion for convergence
        """
        self.order = n
        self.nsteps = nstep
        self.max_iter = number
        self.accuracy = accuracy
        #
        self.infinite_depth = False
        #
        self._labels: list = [ ]
        self._cases: list = [ ]
        self._current = MeanCurrent()
    #
    def __getitem__(self, case_name: int|str) -> tuple:
        """
        case_name : Wave name
        """
        try:
            index = self._labels.index(case_name)
            return self._cases[index]
        except ValueError:
            raise IndexError('   *** wave {:} does not exist'.format(case_name))
    #
    #
    @property
    def infinite_water_depth(self):
        """ Water of infinite depth"""
        self.infinite_depth = True
    #
    @property
    def mean_current(self):
        """ """
        return self._current
#
#
#
#
#
#def to_matrix(l, n):
#    return [l[i:i+n] for i in range(0, len(l), n)]
#
def get_wave_data(case_data):
    """ """
    if isinstance(case_data, (list, tuple)):
        data = get_data_list(data=case_data)
    elif isinstance(case_data, dict):
        data = get_data_dic(data=case_data)
    else:
        raise IOError('input data not valid')
    return data
#
def get_data_dic(data:dict)->list[float]:
    """[case, load, theta, phi, rho, phase]"""
    new_data = [0,0,0,0, "Fourier"]
    for key, item in data.items():
        if re.match(r"\b((wave(\_)?)?h(eight)?(w)?)\b", key, re.IGNORECASE):
            new_data[0] = item.value
        elif re.match(r"\b((wave(\_)?)?period|t(w)?|s)\b", key, re.IGNORECASE):
        #elif re.match(r"\b(t(w)?|s)\b", key, re.IGNORECASE):
            new_data[1] = item.value
        elif re.match(r"\b((water(\_)?)?d(epth)?)\b", key, re.IGNORECASE):
            new_data[2] = item.value
        elif re.match(r"\b((wave(\_)?)?l(ength)?)\b", key, re.IGNORECASE):
            new_data[3] = item.value
        elif re.match(r"\b((wave(\_)?)?t(ype|heory))\b", key, re.IGNORECASE):
            new_data[4] = item
        #elif re.match(r"\b(phase)\b", key, re.IGNORECASE):
        #    new_data[5] = item.value        
    return new_data
#
#
def get_data_list(data:list, steps:int=5)->list[float]:
    """
    [Hw, Tw, d, Lw, wave_type]
    """
    new_data = [0] * steps
    new_data[-1] = "Fourier"
    for x, item in enumerate(data):
        try:
            new_data[x] = item.value
        except AttributeError:
            new_data[-1] = item
    return new_data
#
#